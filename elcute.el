;;; elcute.el --- Commands for marking and killing lines electrically  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Vili Aapro

;; Author: Vili Aapro
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.1
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

;; Elcute is a library defining the minor mode `elcute-mode' for
;; marking and killing lines electrically in the spirit of Paredit.
;; Lines are marked and killed rounding up to whole expressions,
;; without escaping any containing expression.

;; Supported major modes are Lisp Data mode, Inferior Lisp mode, nXML
;; mode, SML mode and Inferior SML mode.  The library is scarcely
;; useful in other modes; in particular, the commands do not work in C
;; mode.

;;; Code:

(require 'cl-lib)
(require 'nxml-mode)

(defvar elcute-init-alist
  `((lisp-data-mode ,#'elcute--lisp-init)
    (inferior-lisp-mode ,#'elcute--lisp-init)
    (nxml-mode ,#'elcute--nxml-init)
    (sml-mode ,#'elcute--sml-init)
    (inferior-sml-mode ,#'elcute--sml-init))
  "Associates modes with lists of initialization functions.")

(defun elcute--init (alist)
  (dolist (pair alist)
    (cl-destructuring-bind (mode . functions) pair
      (when (derived-mode-p mode)
	(dolist (f functions)
	  (funcall f))))))

(defun elcute--lisp-init ()
  (setq-local
   elcute-creep-forward-function #'elcute--lisp-creep-forward
   elcute-creep-backward-function #'elcute--lisp-creep-backward))

(defun elcute--nxml-init ()
  (setq-local
   elcute-stop-predicate #'elcute--nxml-stop-p
   elcute-context-function #'elcute--nxml-context
   elcute-string-skip-function #'elcute--nxml-string-skip
   elcute-error-inside-comment-flag nil))

(defun elcute--sml-init ()
  (setq-local
   elcute-creep-forward-function #'elcute--sml-creep-forward
   elcute-creep-backward-function #'elcute--sml-creep-backward))

;;;###autoload
(define-minor-mode elcute-mode
  "Toggle electric killing of lines (Elcute mode) in the current buffer.

When Elcute mode is enabled, `kill-line' is remapped to
`elcute-kill-line', and various buffer-local variables are set
according to major mode, affecting the commands
`elcute-forward-line', `elcute-mark-line' and `elcute-kill-line'.

Supported major modes are Lisp Data mode, Inferior Lisp mode, nXML mode,
SML mode and Inferior SML mode."
  :keymap (define-keymap
	    "<remap> <kill-line>" #'elcute-kill-line)
  (elcute--init elcute-init-alist))

(defvar elcute-stop-predicate (cl-constantly nil)
  "Should we stop at tentative position?")

(defun elcute-stop-predicate ()
  "Should we stop at tentative position?"
  (funcall elcute-stop-predicate))

(defun elcute--nxml-stop-p ()
  "Refine motion in `elcute-forward-line' in nXML mode to lines.

Instead of moving full nodes, stop if `xmltok-type' is `'data' or
`'comment'.  In nXML mode, `elcute--default-creep-forward' and
`elcute--default-creep-backward' modify `xmltok-type' through
`forward-sexp' and `backward-sexp', respectively."
  (memq xmltok-type '(data comment)))

(defun elcute--context ()
  (syntax-ppss-context (syntax-ppss)))

(defvar elcute-context-function #'elcute--context
  "Returns syntactic context at point.")

(defun elcute-context-function ()
  "Return syntactic context at point."
  (funcall elcute-context-function))

(defun elcute--nxml-context ()
  "Return syntactic context at point translated by XML token type."
  (let ((context (elcute--context)))
    (if (eq context 'string)
	(progn
	  (nxml-token-after)
	  (cl-case xmltok-type
	    (start-tag context)
	    (empty-element context)
	    (data nil)
	    (t (user-error "Inside unrecognized token"))))
      context)))

(defvar elcute-string-skip-function
  #'elcute--lisp-string-skip
  "Skips within syntactic string.")

(defun elcute-string-skip-function (sign limit)
  "Skip within syntactic string.
SIGN specifies direction: when positive, skip forward; when
negative, backward.  LIMIT specifies position beyond which not to
skip."
  (funcall elcute-string-skip-function sign limit))

(defun elcute--lisp-string-skip (sign limit)
  (let ((string-start (nth 8 (syntax-ppss))))
    (cl-case (cl-signum sign)
      (+1 (goto-char
	   (min limit
		(elcute--excurse
		  (goto-char string-start)
		  (condition-case nil
		      (forward-sexp)
		    (error (goto-char (point-max)))
		    (:success (backward-char)))))))
      (-1 (goto-char
	   (max limit
		(elcute--excurse
		  (goto-char string-start)
		  (forward-char))))))))

(defun elcute--nxml-string-skip (sign limit)
  (let* ((terminator (nth 3 (syntax-ppss)))
	 (chars (cl-case terminator
		  (?\' "^'")
		  (?\" "^\""))))
    (cl-case (cl-signum sign)
      (+1 (skip-chars-forward chars limit))
      (-1 (skip-chars-backward chars limit)))))

(defvar elcute-error-inside-comment-flag t
  "Non-nil means `elcute-forward-line' raises an error inside comments.")

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
  "Creeps forward.")
(defvar elcute-creep-backward-function #'elcute--default-creep-backward
  "Creeps backward.")

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

(defun elcute--sml-creep-forward (limit)
  (while (and (< (point) limit)
	      (or (< 0 (skip-chars-forward "[:blank:]\n" limit))
		  (forward-comment +1)
		  (elcute--try #'forward-sexp)))))

(defun elcute--sml-creep-backward (limit)
  (while (and (> (point) limit)
	      (or (> 0 (skip-chars-backward "[:blank:]\n" limit))
		  (forward-comment -1)
		  (elcute--try #'backward-sexp)))))

(defmacro elcute--excurse (&rest body)
  (declare (indent 0))
  `(save-excursion
     ,@body
     (point)))

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
  ;; nXML mode happens to work without taking `min' and `max' inside
  ;; text nodes, but not inside comment nodes.
  (let ((sign (if (< 0 (or arg 1)) +1 -1)))
    (cl-multiple-value-bind (min-or-max creep)
	(cl-case sign
	  (+1 (cl-values #'min elcute-creep-forward-function))
	  (-1 (cl-values #'max elcute-creep-backward-function)))
      (let ((limit (elcute--excurse (elcute--tentative-forward-line arg)))
	    (context (elcute-context-function)))
	(cond
	 ((eq context 'string)
	  (elcute-string-skip-function sign limit))
	 ((and (eq context 'comment)
	       elcute-error-inside-comment-flag)
	  (user-error "Inside comment"))
	 (t
	  (let ((pos (elcute--excurse (funcall creep limit))))
	    (goto-char
	     (if (elcute-stop-predicate)
		 (funcall min-or-max pos limit)
	       pos)))))))))

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

(defun elcute--skip-indentation ()
  (when (elcute--indent-p)
    (goto-char
     (max (point)
	  (elcute--excurse
	    (beginning-of-line)
	    (skip-chars-forward "[:blank:]"))))))

(defun elcute--skip-whitespace ()
  (when (elcute--indent-p)
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
  (elcute--skip-indentation)
  (save-excursion
    (let ((origin (point)))
      (elcute-forward-line arg)
      (elcute--skip-whitespace)
      (kill-region origin (point)))))

(provide 'elcute)

;;; elcute.el ends here
