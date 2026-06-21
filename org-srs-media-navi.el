;;; org-srs-media-navi.el --- Item navigation during media review -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bohong Huang

;; Author: Bohong Huang <bohonghuang@qq.com>
;; Maintainer: Bohong Huang <bohonghuang@qq.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (org-srs "1.0") (mpvi) (subed) (emms) (gptel))
;; URL: https://github.com/bohonghuang/org-srs-media
;; Keywords: multimedia, outline

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Item navigation support during `media+front+back' card review.
;; Provides `org-srs-media-navi-mode' with convenient key bindings
;; for moving between list items and org headings.

;;; Code:

(require 'org-srs-review)

(defun org-srs-media-navi-parent-item ()
  "Move to the parent of the current list item."
  (interactive)
  (let ((item (org-in-item-p)))
    (unless item (error "Not in an item"))
    (let* ((struct (org-list-struct))
           (parents (org-list-parents-alist struct))
           (parent (org-list-get-parent item struct parents)))
      (if parent
          (goto-char parent)
        (error "No parent item")))))

(defun org-srs-media-navi-first-child-item ()
  "Move to the first child of the current list item."
  (interactive)
  (let ((item (org-in-item-p)))
    (unless item (error "Not in an item"))
    (let* ((struct (org-list-struct))
           (child (org-list-has-child-p item struct)))
      (if child
          (goto-char child)
        (error "No child item")))))

;;;###autoload
(defun org-srs-media-navi-up ()
  "Move to the previous item."
  (interactive)
  (org-previous-item))

;;;###autoload
(defun org-srs-media-navi-down ()
  "Move to the next item."
  (interactive)
  (org-next-item))

;;;###autoload
(defun org-srs-media-navi-left ()
  "Move to the parent of the current list item."
  (interactive)
  (org-srs-media-navi-parent-item))

;;;###autoload
(defun org-srs-media-navi-right ()
  "Move to the first child of the current list item."
  (interactive)
  (org-srs-media-navi-first-child-item))

;;;###autoload
(defun org-srs-media-navi-a ()
  "Rate the current review item as good."
  (interactive)
  (org-srs-review-rate-good))

;;;###autoload
(defun org-srs-media-navi-b ()
  "Rate the current review item as again."
  (interactive)
  (org-srs-review-rate-again))

;;;###autoload
(defun org-srs-media-navi-x ()
  "Rate the current review item as easy."
  (interactive)
  (org-srs-review-rate-easy))

;;;###autoload
(defun org-srs-media-navi-y ()
  "Rate the current review item as hard."
  (interactive)
  (org-srs-review-rate-hard))

;;;###autoload
(defun org-srs-media-navi-select ()
  "Explain the current media entry with gptel."
  (interactive)
  (org-srs-media-explain-this-entry))

;;;###autoload
(defun org-srs-media-navi-start ()
  "Toggle review on and off."
  (interactive)
  (if (org-srs-reviewing-p)
      (org-srs-review-quit)
    (org-srs-review-start)))

;;;###autoload
(define-minor-mode org-srs-media-navi-mode
  "Minor mode for navigating org items during media review.
\\{org-srs-media-navi-mode-map}"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<KEYCODE_DPAD_UP>") #'org-srs-media-navi-up)
            (define-key map (kbd "<KEYCODE_DPAD_DOWN>") #'org-srs-media-navi-down)
            (define-key map (kbd "<KEYCODE_DPAD_LEFT>") #'org-srs-media-navi-left)
            (define-key map (kbd "<KEYCODE_DPAD_RIGHT>") #'org-srs-media-navi-right)
            (define-key map (kbd "<KEYCODE_BUTTON_A>") #'org-srs-media-navi-a)
            (define-key map (kbd "<KEYCODE_BUTTON_B>") #'org-srs-media-navi-b)
            (define-key map (kbd "<KEYCODE_BUTTON_X>") #'org-srs-media-navi-x)
            (define-key map (kbd "<KEYCODE_BUTTON_Y>") #'org-srs-media-navi-y)
            (define-key map (kbd "<KEYCODE_BUTTON_START>") #'org-srs-media-navi-start)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT>") #'org-srs-media-navi-select)
            map))

(defun org-srs-media-navi-setup (type &rest _args)
  "Enable `org-srs-media-navi-mode' for `media+front+back' TYPE review items."
  (when (eq type 'media+front+back)
    (org-srs-media-navi-mode +1)
    (org-srs-review-add-hook-once
     'org-srs-review-continue-hook
     (lambda () (org-srs-media-navi-mode -1)))))

(add-hook 'org-srs-item-before-review-hook #'org-srs-media-navi-setup)

(provide 'org-srs-media-navi)
;;; org-srs-media-navi.el ends here
