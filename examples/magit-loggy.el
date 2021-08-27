;;; magit-loggy.el --- Group Magit log commits by date  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

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

;; This works, but since magit-log uses overlays for the committer and
;; date on each line, those are lost when we erase the buffer.  Fixing
;; that would require copying the properties of those overlays into
;; the text properties of each line.  Probably not worth it, since
;; this is just a demo, anyway.

;;; Code:

;;;; Requirements

(require 'magit-log)
(require 'ov)

;;;; Variables

(defvar magit-loggy-taxy
  (make-taxy :name "Commits"
             :taxys (list
                     (make-taxy :name "By date"
                                :take (apply-partially #'taxy-take-keyed #'cdr)))))

;;;; Customization


;;;; Commands

(defun magit-loggy ()
  (interactive)
  (cl-assert (derived-mode-p 'magit-log-mode))
  (save-excursion
    (goto-char (point-min))
    (cl-labels ((line-date
                 () (when-let ((ov (car (ov-in 'before-string 'any (line-beginning-position) (line-end-position))))
                               (string (cadr (get-text-property 0 'display (overlay-get ov 'before-string))))
                               (_ (string-match (rx (group (1+ digit) ; number
                                                           " "
                                                           (1+ (not blank))) ; unit
                                                    (1+ blank) eos)
                                                string)))
                      (match-string 1 string))))
      (let* ((lines (cl-loop until (eobp)
                             collect (cons (buffer-substring (point-at-bol) (point-at-eol))
                                           (line-date))
                             do (forward-line 1)))
             (taxy (taxy-fill lines (taxy-emptied magit-loggy-taxy)))
             (inhibit-read-only t))
        (erase-buffer)
        (taxy-magit-section-insert taxy)))))


;;;; Functions


;;;; Footer

(provide 'magit-loggy)

;;; magit-loggy.el ends here
