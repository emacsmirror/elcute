;;; slurpbarf.el --- Commands for slurping and barfing  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Vili Aapro

;; Author: Vili Aapro
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0
;; URL: https://codeberg.org/vilij/slurpbarf-elcute

;; This file is part of Slurpbarf.
;;
;; Slurpbarf is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.
;;
;; Slurpbarf is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Slurpbarf.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Slurpbarf a library for slurping and barfing, or ingesting and
;; emitting, expressions in the spirit of Paredit.  The behaviour of
;; commands is not exactly identical to that in Paredit: in
;; particular, negative arguments are handled differently in
;; Slurpbarf.

;; Supported major modes are Lisp Data mode and nXML mode; however,
;; the commands should mostly work also in C mode and others; a global
;; minor mode is provided.

;; A command for splicing expressions, `slurpbarf-splice', is included
;; but not bound to any key.

;;; Code:

(require 'cl-lib)
(require 'nxml-mode)

;;;###autoload
(define-minor-mode slurpbarf-mode
  "Toggle slurping and barfing (Slurpbarf mode) in the current buffer.

When Slurpbarf mode is enabled, various buffer-local variables
are set according to major mode, affecting the commands bound in
the mode map, and the command `slurpbarf-splice'.

Supported major modes are `lisp-data-mode' and `nxml-mode' along
with their derivatives; however, the default Lisp Data behavior
may be useful globally."
  :keymap (define-keymap
	    "C-)" #'slurpbarf-slurp-forward
	    "C-(" #'slurpbarf-slurp-backward
	    "C-}" #'slurpbarf-barf-forward
	    "C-{" #'slurpbarf-barf-backward)
  (when (derived-mode-p 'nxml-mode)
    (setq-local
     slurpbarf-insert-space-flag nil
     slurpbarf-skip-comments-flag nil
     slurpbarf-up-function #'slurpbarf--nxml-up
     slurpbarf-down-function #'slurpbarf--nxml-down
     slurpbarf-forward-function #'slurpbarf--nxml-forward)))

;;;###autoload
(define-globalized-minor-mode global-slurpbarf-mode
  slurpbarf-mode slurpbarf-mode
  :group 'convenience)

(defvar slurpbarf-insert-space-flag t
  "Non-nil means insert a space between words and symbols.")
(defvar slurpbarf-skip-comments-flag t
  "Non-nil means skip comments when barfing.")
(defvar slurpbarf-up-function #'slurpbarf--lisp-up
  "Goes up expression hierarchy.")
(defvar slurpbarf-down-function #'down-list
  "Goes down expression hierarchy.")
(defvar slurpbarf-forward-function #'slurpbarf--lisp-forward
  "Moves forward expressions laterally.")

(defun slurpbarf-up-function (&optional arg interactive)
  "Go up |ARG| levels of expression hierarchy.
With positive argument, move forward; with negative, backward.
If INTERACTIVE is non-nil, as it is interactively, report errors
as appropriate for this kind of usage."
  (interactive "p\nd")
  (funcall slurpbarf-up-function arg interactive))

(defun slurpbarf-down-function (&optional arg interactive)
  "Go down |ARG| levels of expression hierarchy.
With positive argument, move forward; with negative, backward.
If INTERACTIVE is non-nil, as it is interactively, report errors
as appropriate for this kind of usage."
  (interactive "p\nd")
  (funcall slurpbarf-down-function arg interactive))

(defun slurpbarf-forward (&optional arg interactive)
  "Move forward ARG expressions or as far as we can.
With negative argument, move backward.  If INTERACTIVE is
non-nil, as it is interactively, report errors as appropriate for
this kind of usage.

Return number of expressions moved."
  (interactive "p\nd")
  (let ((sign (cl-signum arg))
	(n (abs arg))
	(i 0)
	(pos (point)))
    (while (and
	    (< i n)
	    (progn
	      (condition-case nil
		  (funcall slurpbarf-forward-function sign interactive)
		(error nil))
	      (/= pos (point))))
      (cl-incf i)
      (setq pos (point)))
    (* sign i)))

(defmacro slurpbarf--excurse (&rest body)
  (declare (indent 0))
  `(save-excursion
     ,@body
     (point)))

(defun slurpbarf--break-out-string ()
  (when (eq (syntax-ppss-context (syntax-ppss)) 'string)
    (skip-syntax-backward "^\"")
    (backward-char)))

(defun slurpbarf--lisp-up (n interactive)
  "Move up N levels of expressions in Lisp Data.
Treat strings as atoms and include prefix characters.  If
INTERACTIVE is non-nil, report errors as appropriate for
interactive usage."
  (when (/= n 0)
    (let ((pos
	   (slurpbarf--excurse
	     (slurpbarf--break-out-string)
	     (up-list n nil interactive)
	     (backward-prefix-chars))))
      (goto-char pos))))

(defun slurpbarf--lisp-forward (n interactive)
  "Move forward N expressions in Lisp Data.
Handle the edge case of `scan-sexps' ending up inside an
unterminated comment at the end of buffer.  If INTERACTIVE is
non-nil, report errors as appropriate for interactive usage."
  (cl-labels ((scan ()
		(scan-sexps (point) n))
	      (complain ()
		(user-error
		 (if (> n 0) "No next sexp" "No previous sexp"))))
    (let ((pos (if interactive
		   (condition-case nil
		       (scan)
		     (scan-error (complain)))
		 (scan))))
      (cond
       ((not pos)
	(if interactive (complain)
	  (signal (if (> n 0) 'end-of-buffer 'beginning-of-buffer) nil)))
       ((and (eq pos (point-max))
	     (eq (save-excursion (syntax-ppss-context (syntax-ppss pos)))
		 'comment))
	(if interactive (complain) (error "Unterminated comment")))
       (t (goto-char pos))))))

(defun slurpbarf--skip-blanks-and-newline ()
  "Skip blanks and a newline.
For stylistic reasons, we consider such trailing whitespace an
inseparable part of an XML tag."
  (skip-chars-forward "[:blank:]")
  (skip-chars-forward "\n" (1+ (point))))

(defmacro slurpbarf--nxml-motion (interactive &rest body)
  "Tame nXML-mode motion by skipping spaces and translating errors.
If INTERACTIVE is non-nil, translate errors raised inside BODY
into user errors."
  (declare (indent 1))
  `(progn
     (if ,interactive
	 (condition-case err (progn ,@body)
	   (error (signal 'user-error (cdr err))))
       (progn ,@body))
     (slurpbarf--skip-blanks-and-newline)))

(defun slurpbarf--nxml-up (n interactive)
  (slurpbarf--nxml-motion interactive
    (nxml-up-element n)))

(defun slurpbarf--nxml-down (n interactive)
  (slurpbarf--nxml-motion interactive
    (nxml-down-element n)))

(defun slurpbarf--nxml-forward (n interactive)
  (slurpbarf--nxml-motion interactive
    (nxml-forward-balanced-item n)))

(defun slurpbarf--indent-p ()
  (and electric-indent-mode
       (not electric-indent-inhibit)))

(defun slurpbarf--indent (n)
  "Indent N levels of containing expressions.
Restrict indentation to field at point.  Do nothing if Electric
Indent is disabled."
  (when (slurpbarf--indent-p)
    (with-restriction (field-beginning) (field-end)
      (cl-labels
	  ((excurse (n extreme)
	     (slurpbarf--excurse
	       (condition-case nil
		   (slurpbarf-up-function n)
		 (error (goto-char extreme))))))
	(let ((left (excurse (- n) (point-min)))
	      (right (excurse n (point-max))))
	  (indent-region left right))))))

(defun slurpbarf--unindent (pos)
  "Delete horizontal whitespace after POS if POS starts a line.
Do nothing if Electric Indent is disabled."
  (when (slurpbarf--indent-p)
    (save-excursion
      (goto-char pos)
      (when (bolp)
	(let ((origin (point)))
	  (skip-chars-forward "[:blank:]")
	  (delete-region origin (point)))))))

(defconst slurpbarf--word-or-symbol-class (list 2 3))

(defun slurpbarf--insert-space ()
  "Insert a space between words and symbols."
  (when
      (and
       slurpbarf-insert-space-flag
       (let ((before (syntax-class (syntax-after (1- (point)))))
	     (after (syntax-class (syntax-after (point)))))
	 (and (memq before slurpbarf--word-or-symbol-class)
	      (memq after slurpbarf--word-or-symbol-class))))
    (insert " ")))

(defun slurpbarf--insert (string)
  (slurpbarf--unindent (point))
  (save-excursion
    (let ((origin (point)))
      (insert string)
      (slurpbarf--insert-space)
      (goto-char origin)
      (slurpbarf--insert-space))))

(defun slurpbarf--extract-region (beg end)
  (let ((beg (min beg end))
	(end (max beg end)))
    (slurpbarf--unindent end)
    (let ((end-margin (- (point-max) end)))
      (slurpbarf--unindent beg)
      (let ((end (- (point-max) end-margin)))
	(delete-and-extract-region beg end)))))

(defun slurpbarf--skip-comments (sign)
  (when slurpbarf-skip-comments-flag
    (forward-comment (* sign (buffer-size)))))

(defun slurpbarf-slurp-forward (&optional arg interactive)
  "Slurp or ingest forward ARG expressions.
With negative argument, slurp backward.  If INTERACTIVE is
non-nil, as it is interactively, report errors as appropriate for
this kind of usage.

Explanation in `ert' syntax (see info node `(ert)erts files'):
=-=
((foo | bar) baz)
=-=
((foo | bar baz))
=-=-="
  (interactive "p\nd")
  (let ((sign (cl-signum arg)))
    (save-excursion
      (slurpbarf-up-function sign interactive)
      (let ((origin (point))
	    (offset
	     (- (slurpbarf--excurse
		  (slurpbarf-down-function (- sign)))
		(point))))
	(slurpbarf-forward arg interactive)
	(when (/= (point) origin)
	  (let ((substring (slurpbarf--extract-region origin (point))))
	    (forward-char offset)
	    (slurpbarf--insert substring))
	  (slurpbarf--indent 2))))))

(defun slurpbarf-slurp-backward (&optional arg interactive)
  "Slurp or ingest backward ARG expressions.
With negative argument, slurp forward.  If INTERACTIVE is
non-nil, as it is interactively, report errors as appropriate for
this kind of usage.

Explanation in `ert' syntax (see info node `(ert)erts files'):
=-=
(foo (bar | baz))
=-=
((foo bar | baz))
=-=-="
  (interactive "p\nd")
  (slurpbarf-slurp-forward (- arg) interactive))

(defun slurpbarf-barf-forward (&optional arg interactive)
  "Barf or emit forward ARG expressions.
With negative argument, barf backward.  If INTERACTIVE is
non-nil, as it is interactively, report errors as appropriate for
this kind of usage.

Explanation in `ert' syntax (see info node `(ert)erts files'):
=-=
((foo | bar baz))
=-=
((foo | bar) baz)
=-=-="
  (interactive "p\nd")
  (let ((sign (cl-signum arg)))
    (save-excursion
      (slurpbarf-up-function sign interactive)
      (let ((offset
	     (- (slurpbarf--excurse
		  (slurpbarf-down-function (- sign)))
		(point))))
	(forward-char offset)
	(let ((origin (point)))
	  (slurpbarf-forward (- arg) interactive)
	  (when (/= (point) origin)
	    (slurpbarf--skip-comments (- sign))
	    (let ((substring (slurpbarf--extract-region origin (point))))
	      (backward-char offset)
	      (slurpbarf--insert substring))
	    (slurpbarf--indent 2)))))))

(defun slurpbarf-barf-backward (&optional arg interactive)
  "Barf or emit backward ARG expressions.
With negative argument, barf forward.  If INTERACTIVE is non-nil,
as it is interactively, report errors as appropriate for this
kind of usage.

Explanation in `ert' syntax (see info node `(ert)erts files'):
=-=
((foo bar | baz))
=-=
(foo (bar | baz))
=-=-="
  (interactive "p\nd")
  (slurpbarf-barf-forward (- arg) interactive))

(defun slurpbarf-splice ()
  "Splice expression at point into containing expression.

Explanation in `ert' syntax (see info node `(ert)erts files'):
=-=
((foo | bar))
=-=
(foo | bar)
=-=-="
  (interactive)
  (save-excursion
    (let ((out0 (slurpbarf--excurse (slurpbarf-up-function -1)))
	  (out1 (slurpbarf--excurse (slurpbarf-up-function +1)))
	  in0 in1)
      (goto-char out0)
      (setq in0 (slurpbarf--excurse (slurpbarf-down-function +1)))
      (goto-char out1)
      (setq in1 (slurpbarf--excurse (slurpbarf-down-function -1)))
      (delete-region in1 out1)
      (slurpbarf--insert-space)
      (goto-char out0)
      (delete-region in0 out0)
      (slurpbarf--insert-space)))
  (slurpbarf--indent 1))

(provide 'slurpbarf)

;;; slurpbarf.el ends here
