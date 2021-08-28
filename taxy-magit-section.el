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

(cl-defstruct (taxy-magit-section (:include taxy))
  ;; This struct is not required to be used for taxys passed to
  ;; `taxy-magit-section-insert', but it allows a visibility function
  ;; to be specified to override the default for it.
  (visibility-fn #'taxy-magit-section-visibility)
  format-fn)

;;;; Commands


;;;; Functions

(cl-defun taxy-magit-section-insert (taxy &key (objects 'first))
  "Insert a `magit-section' for TAXY into current buffer.
If OBJECTS is `first', insert a taxy's objects before its
descendant taxys; if `last', insert them after descendants."
  (let* ((depth 0)
         (magit-section-set-visibility-hook (cons #'taxy-magit-section-visibility magit-section-set-visibility-hook)))
    (cl-labels ((insert-object
                 (object &optional (format-fn (lambda (o) (format "%s" o))))
                 (magit-insert-section (magit-section object)
                   (magit-insert-section-body
                     (insert (make-string (+ 2 (* depth taxy-magit-section-indent)) ? )
                             (funcall format-fn object)
                             "\n"))))
                (insert-taxy
                 (taxy) (let ((magit-section-set-visibility-hook magit-section-set-visibility-hook)
                              (format-fn (cl-typecase taxy
                                           (taxy-magit-section
                                            (taxy-magit-section-format-fn taxy)))))
                          (cl-typecase taxy
                            (taxy-magit-section
                             (when (taxy-magit-section-visibility-fn taxy)
                               (push (taxy-magit-section-visibility-fn taxy) magit-section-set-visibility-hook))))
                          (magit-insert-section (magit-section taxy)
                            (magit-insert-heading
                              (make-string (* depth taxy-magit-section-indent) ? )
                              (propertize (taxy-name taxy) 'face 'magit-section-heading)
                              (format " (%s%s)"
                                      (if (taxy-description taxy)
                                          (concat (taxy-description taxy) " ")
                                        "")
                                      (taxy-size taxy)))
                            (magit-insert-section-body
                              (when (eq 'first objects)
                                (dolist (object (taxy-objects taxy))
                                  (insert-object object format-fn))
                                (mapc #'insert-object (taxy-objects taxy)))
                              (cl-incf depth)
                              (mapc #'insert-taxy (taxy-taxys taxy))
                              (cl-decf depth)
                              (when (eq 'last objects)
                                (dolist (object (taxy-objects taxy))
                                  (insert-object object format-fn))))))))
      (magit-insert-section (magit-section)
        (insert-taxy taxy)))))

(cl-defun taxy-magit-section-pp (taxy &key (objects 'first))
  "Pretty-print TAXY into a buffer with `magit-section' and show it."
  (with-current-buffer (get-buffer-create "*taxy-magit-section-pp*")
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (taxy-magit-section-insert taxy :objects objects))
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
