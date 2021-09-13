;;; elispy.el --- Show symbols defined in an Elisp project  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'taxy-magit-section)

(defgroup elispy nil
  "Show an overview of symbols defined in an Emacs Lisp project."
  :group 'emacs-lisp-mode)

;;;; Keys

(taxy-define-key-definer elispy-define-key elispy-keys "elispy"
  "FIXME: Docstring.")

(elispy-define-key file ()
  (file-relative-name (plist-get item :file) elispy-directory))

(elispy-define-key type ()
  (let* ((form (plist-get item :form))
	 (type (pcase form
		 (`(,(or 'defun 'cl-defun) . ,_)
		  (if (cl-find-if (lambda (form)
				    (pcase form
				      (`(interactive . ,_) t)))
				  form)
		      'command
		    'function))
		 (`(,(or 'defmacro 'cl-defmacro) . ,_)
		  'macro)
		 (`(,car . ,_) car))))
    (when type
      (format "%s" type))))

(defvar elispy-taxy-default-keys
  '(type file))

;;;; Columns

(taxy-magit-section-define-column-definer "elispy")

(elispy-define-column "Definition" (:max-width 45 :face font-lock-function-name-face)
  (let ((form-defines (pcase-exhaustive (cadr (plist-get item :form))
			((and (pred atom) it) it)
			(`(quote ,it) it)
			(`(,it . ,_) it))))
    (format "%s" form-defines)))

(elispy-define-column "Type" (:max-width 25 :face font-lock-type-face)
  (format "%s" (car (plist-get item :form))))

(elispy-define-column "Docstring" (:max-width nil :face font-lock-doc-face)
  (when-let ((docstring
	      (pcase (plist-get item :form)
		(`(,(or 'defun 'cl-defun 'defmacro 'cl-defmacro) ,_name ,_args
		   ,(and (pred stringp) docstring) . ,_)
		 docstring)
		(`(,(or 'defvar 'defvar-local 'defcustom) ,_name ,_value
		   ,(and (pred stringp) docstring) . ,_)
		 docstring)
		(_ ;; Use the first string found, if any.
		 (cl-find-if #'stringp (plist-get item :form))))))
    (replace-regexp-in-string "\n" "  " docstring)))

(unless elispy-columns
  ;; TODO: Automate this or document it
  (setq-default elispy-columns
		(get 'elispy-columns 'standard-value)))

;;;; Variables

(defvar elispy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elispy-RET)
    (define-key map [mouse-1] #'elispy-mouse-1)
    map))

(defvar-local elispy-directory nil
  "Directory relative to which filenames should be expanded.")

(defvar-local elispy-files nil
  "Files shown in the current Elispy buffer.")

;;;; Commands

(cl-defun elispy (&key (project (or (project-current)
				    (cons 'transient default-directory)))
		       (keys elispy-taxy-default-keys)
		       (files elispy-files)
		       (buffer-name (format "*Elispy: %s*"
					    (if files
						(string-join (mapcar #'file-relative-name files) ", ")
					      (file-name-nondirectory
					       (directory-file-name (project-root project))))))
		       visibility-fn display-buffer-action)
  "Show definitions defined in PROJECT or FILES.
Interactively, with PREFIX, show only definitions in current
buffer."
  (interactive (list :files (when current-prefix-arg
			      (list (buffer-file-name)))
		     :keys (if current-prefix-arg
			       (remove 'file elispy-taxy-default-keys)
			     elispy-taxy-default-keys)))
  (let (format-table column-sizes)
    (cl-labels (;; (heading-face
		;;  (depth) (list :inherit (list 'bufler-group (bufler-level-face depth))))
		(elisp-file-p (file) (string-match-p (rx ".el" (optional ".gz") eos) file))
		(file-visible-p (file) (not (string-match-p (rx bos ".") file)))
		(format-item (item) (gethash item format-table))
		(make-fn (&rest args)
			 (apply #'make-taxy-magit-section
				:make #'make-fn
				:format-fn #'format-item
				:heading-indent elispy-taxy-level-indent
				:visibility-fn visibility-fn
				;; :heading-face-fn #'heading-face
				args))
		(form-name
		 (form) (format "%s" (cl-second (plist-get form :form)))))
      (when (get-buffer buffer-name)
	(kill-buffer buffer-name))
      (with-current-buffer (get-buffer-create buffer-name)
	(elispy-mode)
	(setq-local elispy-taxy-default-keys keys
		    elispy-directory (project-root project)
		    elispy-files files
		    default-directory elispy-directory)
	(setf files (cl-reduce #'cl-remove-if-not (list #'elisp-file-p #'file-visible-p)
			       :initial-value (or files (project-files project))
			       :from-end t))
	(cl-assert files nil "No files to show")
	(let* ((forms (apply #'append (mapcar #'elispy--file-forms files)))
	       (taxy (thread-last
			 (make-fn
			  :name "Elispy"
			  :description (format "Definitions in %s:"
					       (if files
						   (string-join (mapcar #'file-relative-name files) ", ")
						 (file-name-nondirectory
						  (directory-file-name (project-root project)))))
			  :take (taxy-make-take-function keys elispy-keys))
		       (taxy-fill forms)
		       (taxy-sort* #'string< #'taxy-name)
		       (taxy-sort #'string< #'form-name)))
	       (taxy-magit-section-insert-indent-items nil)
	       format-cons)
	  (setf format-cons (taxy-magit-section-format-items
			     elispy-columns elispy-column-formatters taxy)
		format-table (car format-cons)
		column-sizes (cdr format-cons)
		;; NOTE: The first column is handled differently.
		header-line-format (taxy-magit-section-format-header column-sizes elispy-column-formatters))
	  (let ((inhibit-read-only t))
	    (save-excursion
	      (taxy-magit-section-insert taxy :items 'last
		;; :initial-depth bufler-taxy-initial-depth
		;; :blank-between-depth bufler-taxy-blank-between-depth
		)))))
      (pop-to-buffer buffer-name display-buffer-action))))

(defun elispy-revert (_ignore-auto _noconfirm)
  "Revert current Elispy buffer."
  (interactive)
  (elispy :display-buffer-action '((display-buffer-same-window))))

(defun elispy-goto-form ()
  "Go to form at point."
  (interactive)
  (pcase-let* (((map :file :pos) (oref (magit-current-section) value)))
    (pop-to-buffer (or (find-buffer-visiting file)
		       (find-file-noselect file)))
    (goto-char pos)
    (backward-sexp 1)))

(defun elispy-mouse-1 (event)
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'elispy-RET))

(defun elispy-RET ()
  (interactive)
  (cl-typecase (oref (magit-current-section) value)
    (taxy-magit-section (call-interactively #'magit-section-cycle))
    (null nil)
    (t (call-interactively #'elispy-goto-form))))

(define-derived-mode elispy-mode magit-section-mode "Elispy"
  :global nil
  (setq-local bookmark-make-record-function #'elispy--bookmark-make-record
	      revert-buffer-function #'elispy-revert))

;;;; Functions

(cl-defun elispy--file-forms (file)
  "Return forms defined in FILE."
  (with-temp-buffer
    (save-excursion
      (insert-file-contents file))
    (cl-loop for form = (ignore-errors
			  (read (current-buffer)))
	     while form
	     when (listp form)
	     collect (list :file file :pos (point) :form form))))

;;;;; Bookmark support

(defvar bookmark-make-record-function)

(defun elispy--bookmark-make-record ()
  "Return a bookmark record for current Elispy buffer."
  (list (concat "Elispy: %s" elispy-directory)
	(cons 'directory elispy-directory)
	(cons 'handler #'elispy--bookmark-handler)))

(defun elispy--bookmark-handler (record)
  "Show Elispy buffer for bookmark RECORD."
  (pcase-let* ((`(,_ . ,(map directory)) record))
    (elispy :project (project-current nil directory))
    (current-buffer)))

(provide 'elispy)
;;; elispy.el ends here
