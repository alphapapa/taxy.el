;;; taxy.el --- Programmable taxonomical grouping for arbitrary objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; URL: <https://github.com/alphapapa/taxy.el>
;; Version: 0.2-pre
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
  name description key objects taxys
  (predicate #'identity) (then #'ignore)
  (make #'make-taxy)
  take)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(defun taxy-fill (objects taxy)
  "Fill TAXY with OBJECTS according to its definition."
  (cl-labels ((apply-object (object taxy)
                            (or (cl-loop for taxy in (taxy-taxys taxy)
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
                                         finally return nil)
                                ;; No sub-taxys took the object: add it to this taxy.
                                (when (funcall (taxy-predicate taxy) object)
                                  (if (taxy-take taxy)
                                      (funcall (taxy-take taxy) object taxy)
                                    (push object (taxy-objects taxy)))))))
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

(cl-defun taxy-take-keyed
    (key-fns object taxy
             &key (key-name-fn #'identity) (then #'ignore))
  "Take OBJECT into TAXY, adding new taxys dynamically and recursively.
Places OBJECT into a taxy in TAXY for the value returned by
KEY-FNS called with OBJECT.  The new taxys are added to TAXY
recursively as necessary.  Each new taxy's name is that returned
by KEY-NAME-FN called with OBJECT.

Each element of KEY-FNS may be a function or a list of functions.
A list of functions creates a \"chain\" of functions: when an
object is matched by the first function in a chain, it is placed
in that chain's taxonomy, and is not \"offered\" to functions
outside of that chain.

For example, if KEY-FNS were:

  '(((lambda (n) (< n 10)) oddp)
    ((lambda (n) (>= n 10)) evenp))

Then a list of numbers from 0-19 would be classified
like (listing numbers on a single line for the sake of example):

  - <10:
    - 0, 2, 4, 6, 8
    - oddp:
      - 1, 3, 5, 7, 9
  - >=10:
    - 11, 13, 15, 17, 19
    - evenp:
      - 10, 12, 14, 16, 18

So only numbers below 10 are tested against `oddp', and only
numbers greater-than-or-equal-to 10 are tested against
`evenp'.  (A contrived example, of course, since testing against
`evenp' or `oddp' is just the inverse.)"
  (declare (indent defun))
  (cl-macrolet ((offer-or-push
                 () `(if (cdr key-fns)
                         (taxy-take-keyed (cdr key-fns) object taxy
                           :key-name-fn key-name-fn :then then)
                       (push object (taxy-objects taxy)))))
    (cl-typecase (car key-fns)
      (function
       ;; A single key function.
       (let ((key-fn (car key-fns)))
         (if-let ((key (funcall key-fn object)))
             ;; This key function returned non-nil for the object:
             ;; apply it to the appropriate sub-taxy.
             (let ((key-taxy
                    (or (cl-find-if (lambda (taxy-key)
                                      (equal key taxy-key))
                                    (taxy-taxys taxy)
                                    :key #'taxy-key)
                        ;; No existing, matching sub-taxy found: make
                        ;; a new one and add it to TAXY's sub-taxys.
                        (car
                         (push (funcall
                                ;; NOTE: Calling `make-taxy' directly might offer the
                                ;; compiler a chance to optimize compared to using `funcall',
                                ;; but allowing taxy structs to specify their own MAKE
                                ;; functions is very helpful when using specialized structs.
                                (taxy-make taxy)
                                :name (funcall key-name-fn key)
                                :key key
                                :predicate (lambda (object)
                                             (equal key (funcall key-fn object)))
                                :take (when (cdr key-fns)
                                        (lambda (object taxy)
                                          (taxy-take-keyed (cdr key-fns) object taxy
                                            :key-name-fn key-name-fn :then then)))
                                :then then)
                               (taxy-taxys taxy))))))
               (if (cdr key-fns)
                   ;; Other key-fns remain: offer object to them, allowing
                   ;; them to create more sub-taxys beneath this key-taxy.
                   (taxy-take-keyed (cdr key-fns) object key-taxy
                     :key-name-fn key-name-fn :then then)
                 ;; No more key-fns remain: add object to this taxy.
                 (push object (taxy-objects key-taxy))))
           ;; No key value: offer to other KEY-FNS or push to this taxy.
           (offer-or-push))))
      (list
       ;; A "chain" of key functions.
       (or (when (funcall (caar key-fns) object)
             ;; The first function in this chain returns non-nil for
             ;; the object: apply the object to the chain.
             (taxy-take-keyed (car key-fns) object taxy
               :key-name-fn key-name-fn :then then))
           ;; This "chain" of key-fns didn't take the object: offer it to
           ;; other chains, or push to this taxy if they don't take it.
           (offer-or-push))))))

(defun taxy-size (taxy)
  "Return the number of objects TAXY holds.
Includes objects in TAXY's sub-taxys."
  (cl-loop for sub-taxy in (taxy-taxys taxy)
           sum (taxy-size sub-taxy) into total
           finally return (+ total (length (taxy-objects taxy)))))

;;;; Footer

(provide 'taxy)

;;; taxy.el ends here
