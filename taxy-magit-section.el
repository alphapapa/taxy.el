;;; taxy-magit-section.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
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


;;;; Commands


;;;; Functions

(cl-defun taxy-magit-section-pp (taxy &key (objects 'first))
  "Pretty-print TAXY into a buffer with `magit-section' and show it."
  (with-current-buffer (get-buffer-create "*taxy-magit-section-pp*")
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (taxy-magit-section-insert taxy :objects objects))
    (pop-to-buffer (current-buffer))))

(cl-defun taxy-magit-section-insert (taxy &key (objects 'first))
  "Insert a `magit-section' for TAXY into current buffer.
If OBJECTS is `first', insert a taxy's objects before its
descendant taxys; if `last', insert them after descendants."
  (let ((depth 0))
    (cl-labels ((insert-object
                 (object) (insert (make-string (+ 2 (* depth taxy-magit-section-indent)) ? )
                                  (format "%s" object)
                                  "\n"))
                (insert-taxy
                 (taxy) (magit-insert-section (magit-section taxy)
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
                              (mapc #'insert-object (taxy-objects taxy)))
                            (cl-incf depth)
                            (mapc #'insert-taxy (taxy-taxys taxy))
                            (cl-decf depth)
                            (when (eq 'last objects)
                              (mapc #'insert-object (taxy-objects taxy)))))))
      (magit-insert-section (magit-section)
        (insert-taxy taxy)))))

;;;; Footer

(provide 'taxy-magit-section)

;;; taxy-magit-section.el ends here
