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
                                     do (progn
                                          (if (taxy-take taxy)
                                              (funcall (taxy-take taxy) object taxy)
                                            (if (taxy-taxys taxy)
                                                (or (apply-object taxy object)
                                                    (push object (taxy-objects taxy)))
                                              (push object (taxy-objects taxy))))
                                          (setf object (funcall (taxy-then taxy) object)))
                                     unless object return t
                                     finally return nil)))
    (dolist (object objects taxy)
      (apply-object taxy object))))

(defun taxy-plain (taxy)
  "Return a list of the human-readable parts of TAXY."
  (delq nil
        (list (taxy-name taxy)
              (taxy-description taxy)
              (taxy-objects taxy)
              (mapcar #'taxy-plain (taxy-taxys taxy)))))

(defun taxy-emptied (taxy)
  "Return a copy of TAXY without objects.
Omits TAXY's objects and those of its descendant taxys.  Useful
when reusing taxy definitions."
  (setf taxy (copy-taxy taxy)
        (taxy-objects taxy) nil
        (taxy-taxys taxy) (mapcar #'taxy-emptied (taxy-taxys taxy)))
  taxy)

(defun taxy-map (fn taxy)
  "Return TAXY, having replaced each object in it with the value of FN on it.
Replaces every object in TAXY and its descendants.  Useful to
replace objects with a more useful form after classification."
  (declare (indent defun))
  ;; It might be preferable to destructively replace objects rather
  ;; than consing new lists, but I haven't found a way that works
  ;; (even `cl-loop' with `in-ref' hasn't worked).
  (setf (taxy-objects taxy) (mapcar fn (taxy-objects taxy))
        (taxy-taxys taxy) (cl-loop for taxy in (taxy-taxys taxy)
                                   collect (taxy-map fn taxy)))
  taxy)

(cl-defun taxy-take-keyed (key-fn object taxy &key (key-name-fn #'identity))
  "Take OBJECT into TAXY, adding new taxys dynamically.
Places OBJECT into a taxy in TAXY for the value returned by
KEY-FN called with OBJECT.  The new taxy's name is that returned
by KEY-NAME-FN called with OBJECT."
  (let* ((key (funcall key-fn object))
         (key-taxy
          (or (cl-find-if (lambda (taxy-key)
                            (equal key taxy-key))
                          (taxy-taxys taxy)
                          :key #'taxy-key)
              (car
               (push (make-taxy
                      :name (funcall key-name-fn key) :key key
                      :predicate (lambda (object)
                                   (equal key (funcall key-fn object))))
                     (taxy-taxys taxy))))))
    (push object (taxy-objects key-taxy))))

;;;; Footer

(provide 'taxy)

;;; taxy.el ends here
