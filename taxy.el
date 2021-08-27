;;; taxy.el --- Programmable taxonomical grouping for arbitrary objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: <https://github.com/alphapapa/taxy.el>
;; Version: 0.1-pre
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

;; This library provides a programmable way to classify arbitrary
;; objects into a hierarchical taxonomy.  (That's a lot of fancy words
;; to say that this lets you put things in nested groups.)

;; Helpful features include:

;; + Dynamic taxonomies: Objects may be classified into hierarchies
;; automatically defined at runtime based on their attributes.

;; + Reusable taxonomies: Taxonomy definitions may be stored in
;; variables and reused in other taxonomies' descendant groups.

;; Basic usage:

;; 1.  Make a taxy with `make-taxy'.
;; 2.  Fill the taxy with objects using `taxy-fill'.
;; 3.  For a simple display of a taxy's objects, use `taxy-plain'.

;; For more details, please see the README.org file.

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
  (cl-labels ((apply-object (object taxy)
                            (cl-loop for taxy in (taxy-taxys taxy)
                                     when (funcall (taxy-predicate taxy) object)
                                     do (progn
                                          (if (taxy-take taxy)
                                              (funcall (taxy-take taxy) object taxy)
                                            (if (taxy-taxys taxy)
                                                (or (apply-object object taxy)
                                                    (push object (taxy-objects taxy)))
                                              (push object (taxy-objects taxy))))
                                          (setf object (funcall (taxy-then taxy) object)))
                                     unless object return t
                                     finally return nil)))
    (dolist (object objects taxy)
      (apply-object object taxy))))

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

(defun taxy-mapcar-objects (fn taxy)
  "Return copy of TAXY, having replaced its objects with the value of FN on each.
Replaces every object in TAXY and its descendants.  Useful to
replace objects with a more useful form after classification."
  (declare (indent defun))
  ;; It might be preferable to destructively replace objects rather
  ;; than consing new lists, but I haven't found a way that works
  ;; (even `cl-loop' with `in-ref' hasn't worked).
  (setf (taxy-objects taxy) (mapcar fn (taxy-objects taxy))
        (taxy-taxys taxy) (cl-loop for taxy in (taxy-taxys taxy)
                                   collect (taxy-mapcar-objects fn taxy)))
  taxy)

(defalias 'taxy-mapcar #'taxy-mapcar-objects)

(defun taxy-mapc-taxys (fn taxy)
  "Return TAXY having applied FN to it and its descendants.
Does not copy TAXY.  Destructively modifies TAXY, if FN does."
  (declare (indent defun))
  (funcall fn taxy)
  (cl-loop for sub-taxy in-ref (taxy-taxys taxy)
           do (setf sub-taxy (taxy-mapc-taxys fn sub-taxy)))
  taxy)

(defalias 'taxy-mapc* #'taxy-mapc-taxys)

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

(cl-defun taxy-take-keyed*
    (key-fns object taxy
             &key (key-name-fn #'identity) (then #'ignore))
  "Take OBJECT into TAXY, adding new taxys dynamically and recursively.
Places OBJECT into a taxy in TAXY for the value returned by
KEY-FNS called with OBJECT.  The new taxys are added to TAXY
recursively as necessary.  Each new taxy's name is that returned
by KEY-NAME-FN called with OBJECT."
  (let ((key-fn (car key-fns)))
    (if-let ((key (funcall key-fn object)))
        (let ((key-taxy (or (cl-find-if (lambda (taxy-key)
                                          (equal key taxy-key))
                                        (taxy-taxys taxy)
                                        :key #'taxy-key)
                            (car
                             (push (make-taxy
                                    :name (funcall key-name-fn key) :key key
                                    :predicate (lambda (object)
                                                 (equal key (funcall key-fn object)))
                                    :take (when (cdr key-fns)
                                            (lambda (object taxy)
                                              (taxy-take-keyed* (cdr key-fns) object taxy)))
                                    :then then)
                                   (taxy-taxys taxy))))))
          (if (cdr key-fns)
              (taxy-take-keyed* (cdr key-fns) object key-taxy)
            (push object (taxy-objects key-taxy))))
      ;; No key value: push to this taxy.
      (if (cdr key-fns)
          (taxy-take-keyed* (cdr key-fns) object taxy)
        (push object (taxy-objects taxy))))))

(defun taxy-size (taxy)
  "Return the number of objects TAXY holds.
Includes objects in TAXY's sub-taxys."
  (cl-loop for sub-taxy in (taxy-taxys taxy)
           sum (taxy-size sub-taxy) into total
           finally return (+ total (length (taxy-objects taxy)))))

;;;; Footer

(provide 'taxy)

;;; taxy.el ends here
