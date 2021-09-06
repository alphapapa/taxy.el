;;; taxy-magit-section.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; Keywords:

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

;;;; Requirements

(require 'taxy)
(require 'magit-section)

;;;; Variables

(defvar taxy-magit-section-indent 2
  "Default indentation per level.")

;;;; Customization


;;;; Structs

;; NOTE: When making `taxy-magit-section' structs at runtime
;; (e.g. with `taxy-take-keyed'), the struct's `make' slot must be set
;; to a function that returns a new struct with the other slots set as
;; desired; the slots' values do not automatically propagate to
;; structs with the default `make' function.  (Using `cl-labels' to
;; define the `make' function makes this simple.)

;; MAYBE: In `taxy-take-keyed', use `taxy-emptied' to copy structs
;; with inheritance for relevant slots, so defining custom `make'
;; functions wouldn't be necessary.

(cl-defstruct (taxy-magit-section
               (:include taxy
                         (make #'make-taxy-magit-section)))
  ;; MAYBE: Pass parent section to the :make function, would make
  ;; inheritance easier (and/or use EIEIO, but that would reduce
  ;; performance, since slot accessors can't be optimized).
  (visibility-fn #'taxy-magit-section-visibility)
  (heading-face (lambda (_depth) 'magit-section-heading))
  (indent 2)
  format-fn)

;;;; Commands


;;;; Functions

(cl-defun taxy-magit-section-insert (taxy &key (items 'first) (initial-depth 0) (blank-between-depth 1))
  "Insert a `magit-section' for TAXY into current buffer.
If ITEMS is `first', insert a taxy's items before its descendant
taxys; if `last', insert them after descendants.  INITIAL-DEPTH
is the initial indentation depth; it may be, e.g. -1 to make the
second level unindented.  BLANK-BETWEEN-DEPTH is the level up to
which blank lines are inserted between sections at that level."
  (let* ((magit-section-set-visibility-hook
          (cons #'taxy-magit-section-visibility magit-section-set-visibility-hook)))
    (cl-labels ((insert-item
                 (item format-fn depth indent)
                 (magit-insert-section (magit-section item)
                   (magit-insert-section-body
		     ;; This is a tedious way to give the indent
		     ;; string the same text properties as the start
		     ;; of the formatted string, but no matter where I
		     ;; left point after using `insert-and-inherit',
		     ;; something was wrong about the properties, and
		     ;; `magit-section' didn't navigate the sections
		     ;; properly anymore.
		     (let* ((formatted (funcall format-fn item))
			    (indent (make-string (+ 2 (* (pcase depth
                                                           ((pred (> 0)) 0)
                                                           (_ depth))
                                                         indent)) ? )))
		       (add-text-properties 0 (length indent)
					    (text-properties-at 0 formatted)
					    indent)
		       (insert indent formatted "\n")))))
                (insert-taxy
                 (taxy depth) (let ((magit-section-set-visibility-hook magit-section-set-visibility-hook)
                                    (format-fn (cl-typecase taxy
                                                 (taxy-magit-section
                                                  (taxy-magit-section-format-fn taxy))
                                                 (t (lambda (o) (format "%s" o))))))
                                (cl-typecase taxy
                                  (taxy-magit-section
                                   (when (taxy-magit-section-visibility-fn taxy)
                                     (push (taxy-magit-section-visibility-fn taxy) magit-section-set-visibility-hook))))
                                (magit-insert-section (magit-section taxy)
                                  (magit-insert-heading
                                    (make-string (* (pcase depth
                                                      ((pred (> 0)) 0)
                                                      (_ depth))
                                                    taxy-magit-section-indent) ? )
                                    (propertize (taxy-name taxy)
                                                'face (funcall (taxy-magit-section-heading-face taxy) depth))
                                    (format " (%s%s)"
                                            (if (taxy-description taxy)
                                                (concat (taxy-description taxy) " ")
                                              "")
                                            (taxy-size taxy)))
                                  (magit-insert-section-body
                                    (when (eq 'first items)
                                      (dolist (item (taxy-items taxy))
                                        (insert-item item format-fn depth (taxy-magit-section-indent taxy))))
                                    (dolist (taxy (taxy-taxys taxy))
                                      (insert-taxy taxy (1+ depth)))
                                    (when (eq 'last items)
                                      (dolist (item (taxy-items taxy))
                                        (insert-item item format-fn depth (taxy-magit-section-indent taxy)))))
                                  (when (<= depth blank-between-depth)
                                    (insert "\n"))))))
      (magit-insert-section (magit-section)
        (insert-taxy taxy initial-depth)))))

(cl-defun taxy-magit-section-pp (taxy &key (items 'first))
  "Pretty-print TAXY into a buffer with `magit-section' and show it."
  (with-current-buffer (get-buffer-create "*taxy-magit-section-pp*")
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (taxy-magit-section-insert taxy :items items))
    (pop-to-buffer (current-buffer))))

(defun taxy-magit-section-visibility (section)
  "Show SECTION if its taxy is non-empty.
Default visibility function for
`magit-section-set-visibility-hook'."
  (pcase (oref section value)
    ((and (pred taxy-p) taxy)
     (pcase (taxy-size taxy)
       (0 'hide)
       (_ 'show)))
    (_ nil)))

;;;; Footer

(provide 'taxy-magit-section)

;;; taxy-magit-section.el ends here
