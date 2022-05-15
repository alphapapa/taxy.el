;; taxy-package-report.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>

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

;; Inspired by Manuel Uberti's blog post:
;; <https://www.manueluberti.eu//emacs/2021/09/01/package-report/>.

;;; Code:

(require 'cl-lib)
(require 'package)

(require 'taxy-magit-section)
(require 'taxy-boxy)

;;;; Options

(defgroup taxy-package-report nil
  "Customization options for taxy-package-report."
  :group 'applications)

(defcustom taxy-package-report-frontend 'taxy-magit-section
  "Which frontend to use to reporting packages."
  :type '(choice (const taxy-magit-section)
                 (const taxy-boxy)))

;;;; Taxy

(defun taxy-package-report ()
  "List installed packages by archive in a `magit-section' buffer."
  (interactive)
  (cl-labels ((package-archive
               (package) (if-let* ((package-struct (car (alist-get (car package) package-archive-contents)))
                                   (archive (package-desc-archive package-struct)))
                             archive
                           "no archive"))
              (format-package
               (package) (symbol-name (car package))))
    (cond
     ((eq taxy-package-report-frontend 'taxy-magit-section)
      (let ((taxy (make-taxy-magit-section
                   :name "Packages by archive"
                   :take (lambda (item taxy)
                           (taxy-take-keyed (list #'package-archive) item taxy))
                   :make (lambda (&rest args)
                           (apply #'make-taxy-magit-section :format-fn #'format-package :indent 0 args))
                   :format-fn #'format-package
                   :indent 0)))
        (taxy-magit-section-pp (taxy-fill package-alist taxy))))
     ((eq taxy-package-report-frontend 'taxy-boxy)
      (let ((taxy (make-taxy-boxy
                   :name "Packages by archive"
                   :take (lambda (item taxy)
                           (taxy-take-keyed (list #'package-archive) item taxy))
                   :make-box
                   (lambda (item)
                     (let* ((pkg (cadr item))
                            (raw-version (package-desc-version pkg))
                            (version (if (listp raw-version)
                                         (string-join
                                          (mapcar #'number-to-string raw-version)
                                          ".")
                                       (number-to-string raw-version)))
                            (dependency (string= "dependency" (package-desc-status pkg))))
                       (boxy-box
                        :name (symbol-name (package-desc-name pkg))
                        :rel (if dependency "behind" "in")
                        :primary (not dependency)
                        :tooltip (concat version "\n"
                                         (boxy-fill-tooltip
                                          (package-desc-summary pkg)))
                        :action `(lambda ()
                                   (interactive)
                                   (find-file ,(package-desc-dir pkg)))))))))
        (taxy-boxy-pp (taxy-fill package-alist taxy)
                      :flex-width 200
                      :default-margin-x 2
                      :default-margin-y 0
                      :default-padding-x 2
                      :default-padding-y 0))))))

;;; taxy-package-report.el ends here
