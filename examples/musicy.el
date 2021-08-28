;;; musicy.el --- View a music library in a useful taxonomy  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
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

;; This is a sample application using `taxi'.  It uses the "mediainfo"
;; program to get info about audio files, but any function could be
;; swapped into its place (e.g. one that retrieved data from MPD).

;;; Code:

;;;; Requirements

(require 'cl-lib)

(require 'taxy)
(require 'taxy-magit-section)

;; Used to avoid repeated calls to "mediainfo" for the same file.
(require 'memoize)

;;;; Variables

(defvar musicy-taxy
  (cl-labels ((call-proc (process &rest args)
                         "Return results of running PROCESS with ARGS."
                         (declare (indent defun))
                         (with-temp-buffer
                           (if (zerop (apply #'call-process process nil t nil
                                             args))
                               (buffer-substring-no-properties (point-min) (point-max))
                             (warn "mediainfo failed for: %S" args))))
              (mediainfo (file)
                         (call-proc "mediainfo" file))
              (mediainfo-attr (attr file)
                              (if-let ((info (musicy-mediainfo file)))
                                  (when (string-match
                                         (rx-to-string `(seq (eval ,attr) (1+ blank) ":" (1+ blank) (group (1+ nonl))))
                                         info)
                                    (match-string 1 info))
                                (format "No info for file: %S" file)))
              (genre (file)
                     (or (mediainfo-attr "Genre" file)
                         "[unknown genre]"))
              (year (file)
                    (or (when-let (date (or (mediainfo-attr "Recorded date" file)
                                            (mediainfo-attr "Original/Released date" file)
                                            (mediainfo-attr "Year" file)))
                          (when (string-match (rx (group (1+ digit))) date)
                            (match-string 1 date)))
                        "[unknown year]"))
              (artist (file)
                      (mediainfo-attr "Performer" file))
              (album (file)
                     (mediainfo-attr "Album" file))
              (track-name (file)
                          (mediainfo-attr "Track name" file))
              (track-number (file)
                            (mediainfo-attr "Track name/Position" file))
              (track-string (file)
                            (concat
                             (pcase (track-number file)
                               ((or "-1" 'nil) nil)
                               (number (format "%s: " number)))
                             (track-name file))))
    (make-taxy
     :name "Musicy"
     :taxys (list (make-taxy
                   :name "Genres"
                   :take (apply-partially #'taxy-take-keyed*
                                          (list #'genre #'artist #'year #'album #'track-string)))))))

;;;; Customization


;;;; Commands

(defun musicy (directory)
  (interactive (list (read-directory-name "Directory of music files: ")))
  (let ((files (directory-files-recursively
                directory (rx "." (or "mp3" "ogg") eos))))
    (musicy-files files)))

(defun musicy-files (files)
  (thread-last musicy-taxy
    taxy-emptied
    (taxy-fill files)
    (taxy-mapc* (lambda (taxy)
                  (setf (taxy-taxys taxy)
                        (cl-sort (taxy-taxys taxy) #'string<
                                 :key #'taxy-name))
                  (setf (taxy-objects taxy)
                        (cl-sort (taxy-objects taxy) #'string<))))
    ;;  taxy-plain
    taxy-magit-section-pp))

;;;; Functions

(defmemoize musicy-mediainfo (file)
  (with-temp-buffer
    (if (zerop (call-process "mediainfo" nil t nil file))
        (buffer-substring-no-properties (point-min) (point-max))
      (warn "mediainfo failed for: %S" file))))

;;;; Footer

(provide 'musicy)

;;; musicy.el ends here
