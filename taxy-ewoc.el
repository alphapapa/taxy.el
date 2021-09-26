;;; taxy-ewoc.el --- View Taxy structs with EWOC     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author:  Adam Porter <adam@alphapapa.net>
;; Keywords: lisp

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

;; This library renders Taxy structs with EWOC.

;; TESTING CODE:

(defvar numbery
  (make-taxy
   :name "Numbery"
   :description "A silly taxonomy of numbers."
   :taxys (list (make-taxy
                 :name "< 10"
                 :description "Numbers below 10 (consuming)"
                 :predicate (lambda (n) (< n 10))
                 :taxys (list
                         ;; These sub-taxys further classify the numbers below 10 into odd
                         ;; and even.  The odd taxy "consumes" numbers, while the even one
                         ;; doesn't, leaving them to reappear in the parent taxy's items.
                         (make-taxy :name "Odd"
                                    :description "(consuming)"
                                    :predicate #'cl-oddp)
                         (make-taxy :name "Even"
                                    :description "(non-consuming)"
                                    :predicate #'cl-evenp
                                    :then #'identity)))
                (make-taxy
                 :name ">= 10"
                 :description "Numbers above 9 (consuming)"
                 :predicate (lambda (n) (>= n 10))
                 :taxys (list
                         ;; Like in the "< 10" taxy, these sub-taxys further classify
                         ;; the numbers, but only one of them consumes numbers it
                         ;; takes in, leaving the rest to reappear in the parent taxy.
                         (make-taxy :name "Divisible by 3"
                                    :description "(non-consuming)"
                                    :predicate (lambda (n) (zerop (mod n 3)))
                                    :then #'identity)
                         (make-taxy :name "Divisible by 4"
                                    :description "(non-consuming)"
                                    :predicate (lambda (n) (zerop (mod n 4)))
                                    :then #'identity)
                         (make-taxy :name "Divisible by 3 or 4"
                                    :description "(consuming)"
                                    ;; Since this taxy's `:then' function is unset,
                                    ;; it defaults to `ignore', which causes it to
                                    ;; consume numbers it takes in.  Since these
                                    ;; numbers have already been taken in (without
                                    ;; being consumed) by the previous two sibling
                                    ;; taxys, they also appear in them.
                                    :predicate (lambda (n) (or (zerop (mod n 3))
                                                               (zerop (mod n 4)))))
                         (make-taxy :name "Divisible by 5"
                                    :description "(non-consuming)"
                                    :predicate (lambda (n) (zerop (mod n 5)))
                                    :then #'identity))))))

(defun taxy-ewoc-test ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Taxy EWOC test*")
    (let* ((inhibit-read-only t)
	   (taxy-ewoc-nosep nil)
	   (taxy-ewoc-footers)
           (taxy (thread-last numbery
		   taxy-emptied
		   (taxy-fill (cl-loop for i below 20
				       collect i))
		   (taxy-sort #'< #'identity)))
	   (header (taxy-ewoc-header taxy))
	   (footer (taxy-ewoc-footer taxy)))
      (read-only-mode)
      (erase-buffer)
      (delete-all-overlays)
      (setq taxy-ewoc-root-ewoc
	    (ewoc-create #'taxy-ewoc--pretty-print header footer taxy-ewoc-nosep))
      (let ((taxy-ewoc-parent-ewoc taxy-ewoc-root-ewoc)
	    (taxy-ewoc-depth (1+ taxy-ewoc-depth)))
	(ewoc-enter-first taxy-ewoc-root-ewoc taxy)))
    (pop-to-buffer (current-buffer))))

;;; Code:

(require 'cl-lib)
(require 'ewoc)

(defgroup taxy-ewoc nil
  "Display Taxy structs with EWOC."
  :group 'taxy)

;;;; Variables

(defvar taxy-ewoc-depth 0)
(defvar taxy-ewoc-level-indent 2)
(defvar taxy-ewoc-nosep nil)
(defvar taxy-ewoc-parent-ewoc nil)
(defvar taxy-ewoc-footers nil)

(defvar-local taxy-ewoc-root-ewoc nil)

;;;; Faces

(defface taxy-ewoc-name
  '((t :inherit diff-hunk-header))
  "Taxy names in EWOC headers.")

(defface taxy-ewoc-description
  '((t :inherit diff-file-header))
  "Taxy descriptions in EWOC headers.")

;;;; Commands

(cl-defun taxy-ewoc-toggle (node (taxy-ewoc-locate taxy-ewoc-root-ewoc)))

;;;; Functions

(cl-defun taxy-ewoc-locate (ewoc &optional (pos (point)))
  "Return node at POS in EWOC.
Looks in child EWOCs."
  (when-let ((node (ewoc-locate ewoc pos)))
    (cl-typecase (ewoc-data node)
      (ewoc (taxy-ewoc-locate (ewoc-data node) pos))
      (t node))))

(cl-defgeneric taxy-ewoc--pretty-print (thing)
  (insert (make-string (* taxy-ewoc-level-indent taxy-ewoc-depth)
                       ? ))
  (prin1 thing (current-buffer)))

(cl-defmethod taxy-ewoc--pretty-print ((taxy taxy))
  (dolist (item (taxy-items taxy))
    (ewoc-enter-last taxy-ewoc-parent-ewoc item))
  (dolist (sub-taxy (taxy-taxys taxy))
    (let* ((indent (make-string (* taxy-ewoc-level-indent
				   taxy-ewoc-depth)
			        ? ))
           (header (concat indent (taxy-ewoc-header sub-taxy)))
           (footer (concat indent (taxy-ewoc-footer sub-taxy)))
           (new-ewoc (ewoc-create #'taxy-ewoc--pretty-print
                                  header footer taxy-ewoc-nosep))
	   (taxy-ewoc-depth (1+ taxy-ewoc-depth)))
      (ewoc-enter-last taxy-ewoc-parent-ewoc new-ewoc)
      (let ((taxy-ewoc-parent-ewoc new-ewoc))
        (ewoc-enter-last new-ewoc sub-taxy)))))

(cl-defmethod taxy-ewoc--pretty-print ((ewoc ewoc))
  (let ((taxy-ewoc-parent-ewoc ewoc))
    (taxy-ewoc--pretty-print (ewoc-data ewoc))))

(cl-defmethod taxy-ewoc--pretty-print ((_function function))
  ;; HACK: Not sure why this is being called for the EWOC printer function itself, but
  ;; this prevents it from being printed.
  nil)

(defun taxy-ewoc-header (taxy)
  "Return header string for TAXY."
  (concat (propertize (or (taxy-name taxy) "")
		      'face 'taxy-ewoc-name)
	  (when (taxy-description taxy)
	    (concat ": " (propertize (taxy-description taxy)
				     'face 'taxy-ewoc-description)))))

(defun taxy-ewoc-footer (taxy)
  "Return footer string for TAXY."
  (when taxy-ewoc-footers
    (concat "FOOTER: " (taxy-name taxy)
            (when (taxy-description taxy)
	      (concat ": " (propertize (taxy-description taxy)
                                       'face 'taxy-ewoc-description))))))

;;;; Footer

(provide 'taxy-ewoc)

;;; taxy-ewoc.el ends here
