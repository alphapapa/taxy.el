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
  name description key objects taxys dynamic
  (predicate #'identity) (then #'ignore)
  take)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(defun taxy-fill (objects taxy)
  "Fill TAXY with OBJECTS according to its definition."
  (cl-labels ((apply-object (taxy object)
                            (cl-loop for taxy in (taxy-taxys taxy)
                                     when (funcall (taxy-predicate taxy) object)
                                     do (setf object (if (taxy-take taxy)
                                                         (progn
                                                           (funcall (taxy-take taxy) object taxy)
                                                           (funcall (taxy-then taxy) object))
                                                       (if (taxy-taxys taxy)
                                                           (progn
                                                             (or (apply-object taxy object)
                                                                 (push object (taxy-objects taxy)))
                                                             (funcall (taxy-then taxy) object))
                                                         (push object (taxy-objects taxy))
                                                         (funcall (taxy-then taxy) object))))
                                     unless object return t
                                     finally return nil)))
    (dolist (object objects taxy)
      (apply-object taxy object))))

(defun taxy-simple (taxy)
  "Return a list of the human-readable parts of TAXY."
  (delq nil
        (list (taxy-name taxy)
              (taxy-description taxy)
              (taxy-objects taxy)
              (mapcar #'taxy-simple (taxy-taxys taxy)))))

(defun taxy-copy (taxy)
  "Return a copy of TAXY without objects.
Clears TAXY's objects and those of its descendant taxys."
  (setf taxy (copy-taxy taxy)
        (taxy-objects taxy) nil
        (taxy-taxys taxy) (mapcar #'taxy-copy (taxy-taxys taxy)))
  taxy)

(defun taxy-apply (fn taxy)
  "Return TAXY, having applied FN to each object in it, including descendants.
Used to apply side effects, e.g. to transform objects into a more
useful form after classification."
  (declare (indent defun))
  ;; I can't seem to find a way to do this without consing new lists.
  ;; Even using `cl-loop' with `in-ref' didn't work.
  (setf (taxy-objects taxy) (mapcar fn (taxy-objects taxy))
        (taxy-taxys taxy) (cl-loop for taxy in (taxy-taxys taxy)
                                   collect (taxy-apply fn taxy)))
  taxy)

;;;; Footer

(provide 'taxy)

;;; taxy.el ends here
