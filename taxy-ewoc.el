;;; taxy-ewoc.el --- View Taxy structs with EWOC     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author:  Adam Porter <adam@alphapapa.net>
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

;; This library renders Taxy structs with EWOC.

;;; Code:

(require 'cl-lib)
(require 'ewoc)

;;;; Variables

(defvar taxy-ewoc-depth 0)
(defvar taxy-ewoc-level-indent 2)

;;;; Faces

(defface taxy-ewoc-name
  '((t :inherit diff-hunk-header))
  "Taxy names in EWOC headers.")

(defface taxy-ewoc-description
  '((t :inherit diff-file-header))
  "Taxy descriptions in EWOC headers.")

;;;; Functions

(cl-defun taxy-ewoc-insert (taxy &key nosep footer
				 (header "HEADER"))
  "Return an EWOC displaying TAXY at point in current buffer.
HEADER, FOOTER, and NOSEP are passed to `ewoc-create', which
see."
  (let* ((indent (make-string (* taxy-ewoc-level-indent
				 taxy-ewoc-depth)
			      ? ))
         (header (concat indent
                         (propertize (or (taxy-name taxy) header)
                                     'face 'taxy-ewoc-name)
		         (when (taxy-description taxy)
			   (concat ": " (propertize (taxy-description taxy)
                                                    'face 'taxy-ewoc-description)))))
         (ewoc (ewoc-create #'taxy-ewoc--pretty-print
			    header footer nosep))
	 (taxy-ewoc-depth (1+ taxy-ewoc-depth)))
    (ewoc-enter-first ewoc taxy)
    ewoc))

(cl-defgeneric taxy-ewoc--pretty-print (thing)
  (insert (make-string (* taxy-ewoc-level-indent taxy-ewoc-depth)
                       ? ))
  (prin1 thing (current-buffer)))

(cl-defmethod taxy-ewoc--pretty-print ((taxy taxy))
  (mapc #'taxy-ewoc--pretty-print (taxy-items taxy))
  (mapc #'taxy-ewoc-insert (taxy-taxys taxy)))

;;;; Footer

(provide 'taxy-ewoc)

;;; taxy-ewoc.el ends here
