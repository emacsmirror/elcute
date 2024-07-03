;;; elcute.el --- Commands for marking and killing lines electrically  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Vili Aapro

;; Author: Vili Aapro
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0
;; URL: https://codeberg.org/vilij/slurpbarf-elcute

;; This file is part of Elcute.
;;
;; Elcute is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; Elcute is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Elcute.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Elcute is a library for marking and killing lines electrically in
;; the spirit of Paredit.  Lines are marked and killed rounding up to
;; whole expressions, without escaping any containing expression.

;; Supported major modes are Lisp Data mode and nXML mode.  The
;; library is scarcely useful in other modes; in particular, the
;; commands do not work in C mode.

;;; Code:

;;;###autoload
(define-minor-mode elcute-mode
  "Kill lines rounding up to whole expressions."
  :keymap (define-keymap
	    "<remap> <kill-line>" #'elcute-kill-line)
  (when (derived-mode-p 'lisp-data-mode)
    (setq-local
     elcute-creep-forward-function #'elcute--lisp-creep-forward
     elcute-creep-backward-function #'elcute--lisp-creep-backward))
  (when (derived-mode-p 'nxml-mode)
    (setq-local
     elcute-stop-predicate #'elcute--nxml-stop-p
     elcute-string-skip-function #'elcute--nxml-string-skip
     elcute-error-inside-comment-flag nil)))

(defvar elcute-stop-predicate (cl-constantly nil)
  "Should we stop at tentative position?")

(defun elcute-stop-predicate ()
  "Should we stop at tentative position?"
  (funcall elcute-stop-predicate))

(defun elcute--nxml-stop-p ()
  "Refine motion in `elcute-forward-line' in nXML mode to lines.

Instead of moving full text nodes, stop if `xmltok-type' is
`'data'.  In nXML mode, `elcute--default-creep-forward' and
`elcute--default-creep-backward' modify `xmltok-type' through
`forward-sexp' and `backward-sexp', respectively."
  (eq xmltok-type 'data))

(defvar elcute-string-skip-function
  #'elcute--lisp-data-string-skip
  "Specifies the method of skipping within syntactic string.")

(defun elcute-string-skip-function (sign limit)
  "Skip within syntactic string.
SIGN specifies direction: when positive, skip forward; when
negative, backward.  LIMIT specifies position beyond which not to
skip."
  (funcall elcute-string-skip-function sign limit))

(defun elcute--lisp-data-string-skip (sign limit)
  (cl-case (cl-signum sign)
    (+1 (skip-syntax-forward "^\"" limit))
    (-1 (skip-syntax-backward "^\"" limit))))

(defun elcute--nxml-string-skip (sign limit)
  (let* ((terminator (nth 3 (syntax-ppss)))
	 (chars (cl-case terminator
		  (?\' "^'")
		  (?\" "^\""))))
    (cl-case (cl-signum sign)
      (+1 (skip-chars-forward chars limit))
      (-1 (skip-chars-backward chars limit)))))

(defvar elcute-error-inside-comment-flag t
  "Non-nil means `elcute-mark-line' raises an error inside comments.")

(defun elcute--tentative-forward-line (arg)
  (if (null arg)
      (if (eolp)
	  (forward-char)
	(end-of-line))
    (forward-line arg)))

(defun elcute--try (function)
  (condition-case nil
      (funcall function)
    (error nil)
    (:success t)))

(defvar elcute-creep-forward-function #'elcute--default-creep-forward
  "Specifies the method of creeping forward.")
(defvar elcute-creep-backward-function #'elcute--default-creep-backward
  "Specifies the method of creeping backward.")

(defun elcute--default-creep-forward (limit)
  (while (and (< (point) limit)
	      (or (< 0 (skip-chars-forward "[:blank:]\n" limit))
		  (elcute--try #'forward-sexp)))))

(defun elcute--default-creep-backward (limit)
  (while (and (> (point) limit)
	      (or (> 0 (skip-chars-backward "[:blank:]\n" limit))
		  (elcute--try #'backward-sexp)))))

(defun elcute--lisp-creep-forward (limit)
  (while (and (< (point) limit)
	      (or (< 0 (skip-chars-forward "[:blank:]\n" limit))
		  (and (forward-comment +1)
		       (skip-chars-backward "\n"))
		  (elcute--try #'forward-sexp)))))

(defun elcute--lisp-creep-backward (limit)
  (while (and (> (point) limit)
	      (or (> 0 (skip-chars-backward "[:blank:]" limit))
		  (forward-comment -1)
		  (> 0 (skip-chars-backward "\n" limit))
		  (elcute--try #'backward-sexp)))))

(defun elcute-forward-line (&optional arg)
  "Move forward ARG lines subject to adjustments.
Round up to whole expressions not escaping any containing
expression.

With null ARG, moves tentatively to end of line,
or to beginning of next line when already at end of line.
With zero ARG, moves tentatively to beginning of line.
With positive ARG, moves tentatively ARG lines forward,
to beginning of line.
With negative ARG, moves tentatively -ARG lines backward,
to beginning of line."
  (interactive "P")
  ;; nXML mode seems to work without taking `min' and `max', but we
  ;; nevertheless use them here for purity's sake.  When bumping into
  ;; a wall, nXML sets `xmltok-type' to something other than 'data,
  ;; causing `elcute-stop-predicate' to return nil.
  (cl-labels
      ((move (sign min-or-max creep)
	 (let ((limit (save-excursion
			(elcute--tentative-forward-line arg)
			(point)))
	       (in-string (nth 3 (syntax-ppss)))
	       (in-comment (nth 4 (syntax-ppss))))
	   (cond (in-string
		  (elcute-string-skip-function sign limit))
		 ((and in-comment elcute-error-inside-comment-flag)
		  (user-error "Inside comment"))
		 (t
		  (let ((pos (save-excursion
			       (funcall creep limit)
			       (point))))
		    (goto-char
		     (if (elcute-stop-predicate)
			 (funcall min-or-max pos limit)
		       pos))))))))
    (if (> (or arg 1) 0)
	(move +1 #'min elcute-creep-forward-function)
      (move -1 #'max elcute-creep-backward-function))))

(defun elcute-mark-line (&optional arg allow-extend)
  "Mark ARG lines subject to adjustments.
Round up to whole expressions not escaping any containing
expression.

When mark is inactive or ALLOW-EXTEND is null, behaves as if mark
were at point.

Interactively, ALLOW-EXTEND is true.

With null ARG and inactive mark, moves mark tentatively to end of
line, or to beginning of next line if already at end of line.

In what follows, always moves mark tentatively to beginning of
line.

With null ARG and active mark, when mark is at or after point,
moves mark tentatively one line forward; when mark is before
point, moves mark tentatively one line backward.

With zero ARG, moves mark tentatively to beginning of line.
With positive ARG, moves mark tentatively ARG lines forward.
With negative ARG, moves mark tentatively -ARG lines backward."
  (interactive "P\np")
  ;; Parts taken from Emacs source for `mark-word'
  (let ((extend (and allow-extend
		     (or (and (eq last-command this-command) (mark t))
			 (region-active-p)))))
    (save-excursion
      (if extend
	  (let ((arg (or arg (let ((sign (if (< (mark) (point)) -1 1)))
			       sign))))
	    (goto-char (mark))
	    (elcute-forward-line arg)
	    (set-mark (point)))
	(progn
	  (elcute-forward-line arg)
	  (push-mark (point) nil t))))))

(defun elcute--indent-p ()
  (and electric-indent-mode
       (not electric-indent-inhibit)))

(defun elcute--indent ()
  (when (elcute--indent-p)
    (indent-according-to-mode)))

(defun elcute--skip-indentation (arg)
  (when (and (elcute--indent-p)
	     (> (or arg 1) 0))
    (skip-chars-forward "[:blank:]")))

(defun elcute-kill-line (&optional arg)
  "Kill ARG lines subject to adjustments.
Round up to whole expressions not escaping any containing
expression.

With null ARG, kills tentatively till end of line,
or till beginning of next line when already at end of line.
With zero ARG, kills tentatively till beginning of line.
With positive ARG, kills tentatively ARG lines forward,
till beginning of line.
With negative ARG, kills tentatively -ARG lines backward,
till beginning of line."
  (interactive "P")
  (save-excursion
    (let ((origin (point)))
      (elcute-forward-line arg)
      (elcute--skip-indentation arg)
      (kill-region origin (point))))
  (elcute--indent))

(provide 'elcute)

;;; elcute.el ends here
