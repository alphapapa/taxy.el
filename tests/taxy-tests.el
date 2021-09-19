;;; taxy-tests.el --- Tests for Taxy                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>

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

;; Tests for Taxy.

;;; Code:

(require 'ert)
(require 'seq)

(require 'taxy)

(defvar taxy-tests-numbery
  (let ((items '(0 1 2 3 4 5 6 7 8)))
    (taxy-fill items
               (make-taxy
                :name "Numbers"
                :taxys (list (make-taxy
                              :name "< 5"
                              :predicate (lambda (n) (< n 5))
                              :taxys (list (make-taxy :name "Odd"
                                                      :predicate #'cl-oddp)
                                           (make-taxy :name "Even"
                                                      :predicate #'cl-evenp)))
                             (make-taxy
                              :name ">= 5"
                              :predicate (lambda (n) (>= n 5))
                              :taxys (list (make-taxy :name "Odd"
                                                      :predicate #'cl-oddp)
                                           (make-taxy :name "Even"
                                                      :predicate #'cl-evenp)))))))
  "A taxy used for testing.")

(ert-deftest taxy-find-parent ()
  (should (equal "Even"
                 (taxy-name
                  (taxy-find-parent 2 taxy-tests-numbery)))))

(ert-deftest taxy-path ()
  (should (equal '("Numbers" "< 5" "Even")
                 (mapcar #'taxy-name
                         (taxy-path 2 taxy-tests-numbery))))
  (should (equal '("Numbers" "< 5" "Odd")
                 (mapcar #'taxy-name
                         (taxy-path 1 taxy-tests-numbery))))
  (should (equal '("Numbers" "< 5")
                 (mapcar #'taxy-name
                         (taxy-path "< 5" taxy-tests-numbery
                                    :key #'taxy-name :test #'equal)))))

(ert-deftest taxy-follow ()
  (should (seq-set-equal-p '(0 2 4)
                           (taxy-items
                            (taxy-follow '("Numbers" "< 5" "Even")
                                         taxy-tests-numbery
                                         :key #'taxy-name)))))

(ert-deftest taxy-plain ()
  (should (equal (taxy-plain taxy-tests-numbery))))

;;; Footer

(provide 'taxy-tests)

;;; taxy-tests.el ends here
