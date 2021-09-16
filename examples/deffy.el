;;; deffy.el --- Show definitions in an Elisp project/buffer  -*- lexical-binding: t; -*-

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

;; This library provides commands that show top-level forms and
;; definitions found in an Emacs Lisp project or buffer, organized by
;; file and type.

;;; Code:

(require 'map)

(require 'taxy)
(require 'taxy-magit-section)

(cl-defstruct deffy-def
  ;; Okay, the name of this struct is silly, but at least it's concise.
  file pos form)

(defgroup deffy nil
  "Show an overview of definitions in an Emacs Lisp project or buffer."
  :group 'emacs-lisp-mode)

;;;; Keys

(taxy-define-key-definer deffy-define-key deffy-keys "deffy"
  "FIXME: Docstring.")

(deffy-define-key file ()
  (file-relative-name (deffy-def-file item) deffy-directory))

(deffy-define-key type ()
  (pcase-let* (((cl-struct deffy-def form) item)
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

(defvar deffy-taxy-default-keys
  '(type file))

;;;; Columns

(taxy-magit-section-define-column-definer "deffy")

(deffy-define-column "Definition" (:max-width 45 :face font-lock-function-name-face)
  (let ((form-defines (pcase-exhaustive (cadr (deffy-def-form item))
			((and (pred atom) it) it)
			(`(quote ,it) it)
			(`(,it . ,_) it))))
    (format "%s" form-defines)))

(deffy-define-column "Type" (:max-width 25 :face font-lock-type-face)
  (format "%s" (car (deffy-def-form item))))

(deffy-define-column "Docstring" (:max-width nil :face font-lock-doc-face)
  (when-let ((docstring
	      (pcase (deffy-def-form item)
		(`(,(or 'defun 'cl-defun 'defmacro 'cl-defmacro) ,_name ,_args
		   ,(and (pred stringp) docstring) . ,_)
		 docstring)
		(`(,(or 'defvar 'defvar-local 'defcustom) ,_name ,_value
		   ,(and (pred stringp) docstring) . ,_)
		 docstring)
		(_ ;; Use the first string found, if any.
		 (cl-find-if #'stringp (deffy-def-form item))))))
    (replace-regexp-in-string "\n" "  " docstring)))

(unless deffy-columns
  ;; TODO: Automate this or document it
  (setq-default deffy-columns
		(get 'deffy-columns 'standard-value)))

;;;; Variables

(defvar deffy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'deffy-RET)
    (define-key map [mouse-1] #'deffy-mouse-1)
    map))

(defvar-local deffy-directory nil
  "Directory relative to which filenames should be expanded.")

(defvar-local deffy-files nil
  "Files shown in the current Deffy buffer.")

(defvar-local deffy-display-buffer-action nil
  "Last-used display-buffer-action in the current Deffy buffer.")

;;;; Options

(defcustom deffy-side-window-action
  '(display-buffer-in-side-window
    (side . right)
    (window-parameters
     (window-side . right)
     (no-delete-other-windows . t)))
  "`display-buffer' action used when displaying Deffy buffer in a side window.
See Info node `(elisp)Displaying Buffers in Side Windows'."
  :type 'sexp)

;;;; Commands

;;;###autoload
(cl-defun deffy (&key (project (or (project-current)
				   (cons 'transient default-directory)))
		      (keys deffy-taxy-default-keys)
		      (files deffy-files)
		      (buffer-name (format "*Deffy: %s*"
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
			       (remove 'file deffy-taxy-default-keys)
			     deffy-taxy-default-keys)))
  (let (format-table column-sizes)
    (cl-labels (;; (heading-face
		;;  (depth) (list :inherit (list 'bufler-group (bufler-level-face depth))))
		(elisp-file-p (file) (string-match-p (rx ".el" (optional ".gz") eos) file))
		(file-visible-p
		 (file) (not (string-match-p (rx bos ".") (file-name-nondirectory file))))
		(format-item (item) (gethash item format-table))
		(make-fn (&rest args)
			 (apply #'make-taxy-magit-section
				:make #'make-fn
				:format-fn #'format-item
				:heading-indent deffy-level-indent
				:visibility-fn visibility-fn
				;; :heading-face-fn #'heading-face
				args))
		(def-name (def) (format "%s" (cl-second (deffy-def-form def)))))
      ;; (when (get-buffer buffer-name)
      ;;   (kill-buffer buffer-name))
      (with-current-buffer (get-buffer-create buffer-name)
	(deffy-mode)
	(setq-local deffy-taxy-default-keys keys
		    deffy-directory (project-root project)
		    deffy-files files
		    deffy-display-buffer-action display-buffer-action
		    default-directory deffy-directory)
	(setf files (cl-reduce #'cl-remove-if-not (list #'elisp-file-p #'file-visible-p)
			       :initial-value (or files (project-files project))
			       :from-end t))
	(cl-assert files nil "No files to show")
	(let* ((forms (apply #'append (mapcar #'deffy--file-forms files)))
	       (taxy (thread-last
			 (make-fn
			  :name "Deffy"
			  :description (format "Definitions in %s:"
					       (if files
						   (string-join (mapcar #'file-relative-name files) ", ")
						 (file-name-nondirectory
						  (directory-file-name (project-root project)))))
			  :take (taxy-make-take-function keys deffy-keys))
		       (taxy-fill forms)
		       (taxy-sort* #'string< #'taxy-name)
		       (taxy-sort #'string< #'def-name)))
	       (taxy-magit-section-insert-indent-items nil)
	       (inhibit-read-only t)
	       (format-cons (taxy-magit-section-format-items
			     deffy-columns deffy-column-formatters taxy)))
	  (setf format-table (car format-cons)
		column-sizes (cdr format-cons)
		header-line-format (taxy-magit-section-format-header
				    column-sizes deffy-column-formatters))
          (delete-all-overlays)
          (erase-buffer)
	  (save-excursion
	    (taxy-magit-section-insert taxy :items 'last
	      ;; :blank-between-depth bufler-taxy-blank-between-depth
	      :initial-depth 0))))
      (pop-to-buffer buffer-name display-buffer-action))))

;;;###autoload
(cl-defun deffy-buffer
    (&optional (buffer (current-buffer))
	       &key display-buffer-action)
  "Show an Deffy view for BUFFER.
Interactively, with prefix, display in dedicated side window."
  (interactive
   (list (current-buffer)
	 :display-buffer-action (when current-prefix-arg
				  deffy-side-window-action)))
  (unless (buffer-file-name buffer)
    (user-error "Buffer is not file-backed: %S.  See command `deffy-project'"
		buffer))
  (deffy :files (list (buffer-file-name buffer))
    :keys (remove 'file deffy-taxy-default-keys)
    :display-buffer-action display-buffer-action))

(cl-defun deffy-project (&optional project &key display-buffer-action)
  "Show an Deffy view for PROJECT.
Interactively, with prefix, display in dedicated side window."
  (interactive
   (list nil :display-buffer-action (when current-prefix-arg
				      deffy-side-window-action)))
  (deffy :project (or project
		      (project-current)
		      (cons 'transient default-directory))
    :display-buffer-action display-buffer-action))

(defun deffy-revert (_ignore-auto _noconfirm)
  "Revert current Deffy buffer."
  (interactive)
  (deffy :display-buffer-action (or deffy-display-buffer-action
				    '((display-buffer-same-window)))))

(defun deffy-goto-form ()
  "Go to form at point."
  (interactive)
  (pcase-let (((cl-struct deffy-def file pos)
	       (oref (magit-current-section) value)))
    (pop-to-buffer
     (or (find-buffer-visiting file)
	 (find-file-noselect file))
     `(display-buffer-in-previous-window
       (previous-window . ,(get-mru-window))))
    (goto-char pos)
    (backward-sexp 1)))

(defun deffy-mouse-1 (event)
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'deffy-RET))

(defun deffy-RET ()
  (interactive)
  (cl-etypecase (oref (magit-current-section) value)
    (deffy-def (call-interactively #'deffy-goto-form))
    (taxy-magit-section (call-interactively #'magit-section-cycle))
    (null nil)))

(define-derived-mode deffy-mode magit-section-mode "Deffy"
  :global nil
  (setq-local bookmark-make-record-function #'deffy--bookmark-make-record
	      revert-buffer-function #'deffy-revert))

;;;; Functions

(cl-defun deffy--file-forms (file)
  "Return forms defined in FILE."
  (with-temp-buffer
    (save-excursion
      (insert-file-contents file))
    (cl-loop for form = (ignore-errors
			  (read (current-buffer)))
	     while form
	     when (listp form)
	     collect (make-deffy-def :file file :pos (point) :form form))))

;;;;; Bookmark support

(defvar bookmark-make-record-function)

(defun deffy--bookmark-make-record ()
  "Return a bookmark record for current Deffy buffer."
  (list (concat "Deffy: %s" deffy-directory)
	(cons 'directory deffy-directory)
	(cons 'files deffy-files)
	(cons 'handler #'deffy--bookmark-handler)))

(defun deffy--bookmark-handler (record)
  "Show Deffy buffer for bookmark RECORD."
  (pcase-let* ((`(,_ . ,(map directory files)) record))
    (deffy :files files :project (project-current nil directory))
    (current-buffer)))

(provide 'deffy)

;;; deffy.el ends here
