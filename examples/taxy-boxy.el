;;; taxy-boxy.el --- View taxy structs as a boxy diagram  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Tyler Grinn <tylergrinn@gmail.com>
;; Maintainer: Tyler Grinn <tylergrinn@gmail.com>
;; URL: https://github.com/alphapapa/taxy.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3") (taxy "0.7") (boxy "1.0"))
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

;; This library provides a way to view `taxy' structs in a boxy diagram.

;;; Code:

;;;; Requirements

(require 'taxy)
(require 'boxy)

;;;; Options

(defgroup taxy-boxy nil
  "Customization options for taxy-boxy"
  :group 'applications)

(defcustom taxy-boxy-margin-x 2
  "Horizontal margin to be used when displaying boxes."
  :type 'number
  :group 'taxy-boxy)

(defcustom taxy-boxy-margin-y 1
  "Vertical margin to be used when displaying boxes."
  :type 'number
  :group 'taxy-boxy)

(defcustom taxy-boxy-padding-x 2
  "Horizontal padding to be used when displaying boxes."
  :type 'number
  :group 'taxy-boxy)

(defcustom taxy-boxy-padding-y 1
  "Vertical padding to be used when displaying boxes."
  :type 'number
  :group 'taxy-boxy)

(defcustom taxy-boxy-flex-width 80
  "When merging links, try to keep width below this."
  :type 'number
  :group 'taxy-boxy)

(defcustom taxy-boxy-default-visibility 2
  "Default level to display boxes."
  :type 'number
  :group 'taxy-boxy)

(defcustom taxy-boxy-tooltips t
  "Show tooltips in a boxy diagram."
  :type 'boolean
  :group 'taxy-boxy)

(defcustom taxy-boxy-tooltip-timeout 0.5
  "Idle time before showing tooltip in a boxy diagram."
  :type 'number
  :group 'taxy-boxy)

(defcustom taxy-boxy-tooltip-max-width 30
  "Maximum width of all tooltips."
  :type 'number
  :group 'taxy-boxy)

;;;; Faces

(defface taxy-boxy-default nil
  "Default face used boxy mode.")

(defface taxy-boxy-primary
   '((((background dark)) (:foreground "turquoise"))
     (t (:foreground "dark cyan")))
   "Face for highlighting the name of a box.")

(defface taxy-boxy-selected
   '((t :foreground "light slate blue"))
  "Face for the current box border under cursor.")

(defface taxy-boxy-rel
  '((t :foreground "hot pink"))
  "Face for the box which is related to the box under the cursor.")

(defface taxy-boxy-tooltip
 '((((background dark)) (:background "gray30" :foreground "gray"))
   (t (:background "gainsboro" :foreground "dim gray")))
 "Face for tooltips in a boxy diagram.")

;;; Structs

(cl-defstruct (taxy-boxy
               (:include taxy
                         (make #'make-taxy-boxy)))
  ;; MAYBE: Pass parent section to the :make function, would make
  ;; inheritance easier (and/or use EIEIO, but that would reduce
  ;; performance, since slot accessors can't be optimized).
  (make-box #'taxy-boxy-make-box))

;;; Taxy interface

(defun taxy-boxy-make-box (item)
  "Make a boxy-box from taxy ITEM.

This function should be reimplemented specifically for each
backend."
  (boxy-box
   ;; (required)
   ;; The name of the item
   :name (prin1-to-string item)

   ;; (required)
   ;; Either 'in', 'on', 'behind', 'in front of', 'above', 'below', 'to the
   ;; right of', or 'to the left of'
   :rel "in"

   ;; (optional)
   ;; Whether to highlight the box's name
   :primary nil

   ;; (optional)
   ;; Tooltip message to show when cursor is in this box.  Limited to
   ;; `boxy-tooltip-max-width' width, but can be any number of lines.  Long lines will be
   ;; truncated.  If not set, the tooltip will display the relationship between this item
   ;; and its parent.  If set to any empty string, don't display tooltip.
   :tooltip ""

   ;; (optional)
   ;; A message to be displayed in the minibuffer when the cursor enters this box
   :help-echo ""

   ;; (optional)
   ;; A list of markers determining where each box should link back to.  Pressing RET on a
   ;; box will take you to the first marker in :markers.  :markers are appended to the
   ;; parent box so the user may cycle through instances without expanding the parent.
   :markers '()))

(cl-defun taxy-boxy-pp (taxy-boxy
                        &key
                        header
                        (display-buffer-fn 'display-buffer-pop-up-window)
                        select
                        (visibility taxy-boxy-default-visibility)
                        (max-visibility 2)
                        (default-margin-x taxy-boxy-margin-x)
                        (default-margin-y taxy-boxy-margin-y)
                        (default-padding-x taxy-boxy-padding-x)
                        (default-padding-y taxy-boxy-padding-y)
                        (flex-width taxy-boxy-flex-width)
                        (tooltips taxy-boxy-tooltips)
                        (tooltip-timeout taxy-boxy-tooltip-timeout)
                        (tooltip-max-width taxy-boxy-tooltip-max-width)
                        (default-face 'taxy-boxy-default)
                        (primary-face 'taxy-boxy-primary)
                        (tooltip-face 'taxy-boxy-tooltip)
                        (rel-face 'taxy-boxy-rel)
                        (selected-face 'taxy-boxy-selected))
  "Pretty print TAXY in a popup buffer.

The MAKE-BOX function will be used to create each box from a taxy
item.

If HEADER is passed in, it will be printed above the diagram.

DISPLAY-BUFFER-FN is used to display the diagram, by
default `display-buffer-pop-up-window'.

If SELECT is non-nil, select the boxy window after displaying
it.

VISIBILITY is the initial visibility of children and
MAX-VISIBILITY is the maximum depth to display when cycling
visibility.

DEFAULT-MARGIN-X, DEFAULT-MARGIN-Y, DEFAULT-PADDING-X and
DEFAULT-PADDING-Y will be the fallback values to use if a box's
margin and padding slots are not set.

When adding boxes, boxy will try to keep the width below
FLEX-WIDTH.

If TOOLTIPS is nil, don't show any tooltips.

TOOLTIP-TIMEOUT is the idle time to wait before showing a
tooltip.

TOOLTIP-MAX-WIDTH is the maximum width of a tooltip.  Lines
longer than this will be truncated.

DEFAULT-FACE, PRIMARY-FACE, TOOLTIP-FACE, REL-FACE, and
SELECTED-FACE can be set to change the appearance of the boxy
diagram."
  (boxy-pp
   (taxy-boxy-to-box taxy-boxy)
   :display-buffer-fn display-buffer-fn
   :visibility visibility
   :max-visibility max-visibility
   :select select
   :header header
   :default-margin-x default-margin-x
   :default-margin-y default-margin-y
   :default-padding-x default-padding-x
   :default-padding-y default-padding-y
   :flex-width flex-width
   :tooltips tooltips
   :tooltip-timeout tooltip-timeout
   :tooltip-max-width tooltip-max-width
   :default-face default-face
   :primary-face primary-face
   :tooltip-face tooltip-face
   :rel-face rel-face
   :selected-face selected-face))

(defun taxy-boxy-to-box (taxy-boxy)
  "Create an `boxy-box' from TAXY.

The MAKE-BOX function will be called to create a box for each
item in TAXY."
  (let ((make-box (taxy-boxy-make-box taxy-boxy))
        (world (boxy-box))
        (box (boxy-box :level 1
                       :name (taxy-name taxy-boxy)
                       :tooltip (if-let ((desc (taxy-description taxy-boxy)))
                                    (boxy-fill-tooltip desc)
                                  ""))))
    (boxy-add-next box world)
    (mapc
     (lambda (item) (taxy-boxy-add-item item box make-box))
     (taxy-items taxy-boxy))
    (mapc
     (lambda (taxy) (taxy-boxy-add-taxy taxy box make-box))
     (taxy-taxys taxy-boxy))
    world))

(defun taxy-boxy-add-taxy (taxy parent make-box)
  "Add TAXY to PARENT.

The MAKE-BOX function will be called to create a box for each
item in TAXY."
  (let ((box (boxy-box :name (taxy-name taxy)
                       :rel "in"
                       :tooltip (if-let ((desc (taxy-description taxy)))
                                     (boxy-fill-tooltip desc)
                                   ""))))
    (boxy-add-next box parent)
    (when-let ((taxys (taxy-taxys taxy)))
      (oset box :expand-children
            `(lambda (box)
               (mapc
                (lambda (taxy) (taxy-boxy-add-taxy taxy box ',make-box))
                ',taxys))))
    (mapc
     (lambda (item) (taxy-boxy-add-item item box make-box))
     (taxy-items taxy))))

(defun taxy-boxy-add-item (item parent make-box)
  "Add ITEM to PARENT using MAKE-BOX to generate a `boxy-box'."
  (let ((box (funcall make-box item)))
    (oset parent :markers (append (if (slot-boundp box :markers) (oref box :markers))
                                  (if (slot-boundp parent :markers) (oref parent :markers))))
    (boxy-add-next box parent)))

(provide 'taxy-boxy)

;;; taxy-boxy.el ends here
