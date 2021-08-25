;;; taxy.el --- Programmable taxonomical grouping for arbitrary objects  -*- lexical-binding: t; -*-

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

(require 'cl-lib)

;;;; Structs

(cl-defstruct taxy
  name description objects
  predicate then taxys)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(defun taxy-apply (taxy objects)
  (cl-labels ((apply-object (taxy object)
                            (cl-loop for taxy in (taxy-taxys taxy)
                                     while object
                                     when (funcall (taxy-predicate taxy) object)
                                     do (setf object
                                              (if-let* ((taxys (taxy-taxys taxy)))
                                                  (apply-object taxy object)
                                                (push object (taxy-objects taxy))
                                                (funcall (taxy-then taxy) object))))))
    (dolist (object objects taxy)
      (apply-object taxy object))))

(defun taxy-simple (taxy)
  (delq nil
        (list (taxy-name taxy)
              (taxy-description taxy)
              (taxy-objects taxy)
              (mapcar #'taxy-print (taxy-taxys taxy)))))

;;;; Footer

(provide 'taxy)

;;; taxy.el ends here
