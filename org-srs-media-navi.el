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

(defun org-srs-media-navi-toggle-playback ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (re-search-forward (org-link-make-regexps) (line-end-position))
    (let ((link-beginning (match-beginning 0)) (link-end (match-end 0)))
      (goto-char link-beginning)
      (org-open-at-point))))

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
  (condition-case nil
      (org-srs-media-navi-parent-item)
    (error
     (org-srs-entry-beginning-of-drawer org-srs-media-explain-drawer-name)
     (beginning-of-line)
     (org-fold-hide-drawer-all))))

;;;###autoload
(defun org-srs-media-navi-right ()
  "Move to the first child of the current list item."
  (interactive)
  (if (org-at-item-p)
      (org-srs-media-navi-first-child-item)
    (org-srs-entry-beginning-of-drawer org-srs-media-explain-drawer-name)
    (org-fold-hide-drawer-toggle 'off)
    (re-search-forward
     (rx bol "- ")
     (save-excursion
       (org-srs-entry-end-of-drawer org-srs-media-explain-drawer-name)
       (point)))
    (beginning-of-line)))

;;;###autoload
(defun org-srs-media-navi-select-a ()
  "Explain the current media entry with gptel."
  (interactive)
  (cl-loop for command = (org-srs-item-confirm-pending-p)
           while command
           do (call-interactively command))
  (org-srs-media-explain-this-entry))

;;;###autoload
(defun org-srs-media-navi-select-b ()
  "Abort the current gptel session."
  (interactive)
  (gptel-abort (current-buffer)))

;;;###autoload
(defun org-srs-media-navi-select-select ()
  "Reset playback speed and volume."
  (interactive)
  (mpvi-volume nil)
  (mpvi-speed nil))

;;;###autoload
(defun org-srs-media-navi-select-l1 ()
  "Decrease playback speed by 0.25."
  (interactive)
  (mpvi-speed "-0.25"))

;;;###autoload
(defun org-srs-media-navi-select-r1 ()
  "Increase playback speed by 0.25."
  (interactive)
  (mpvi-speed "+0.25"))

;;;###autoload
(defun org-srs-media-navi-select-left ()
  "Seek backward 1 second."
  (interactive)
  (mpvi-time "-1"))

;;;###autoload
(defun org-srs-media-navi-select-right ()
  "Seek forward 1 second."
  (interactive)
  (mpvi-time "+1"))

;;;###autoload
(defun org-srs-media-navi-select-up ()
  "Increase volume by 10%."
  (interactive)
  (mpvi-volume "+10"))

;;;###autoload
(defun org-srs-media-navi-select-down ()
  "Decrease volume by 10%."
  (interactive)
  (mpvi-volume "-10"))

;;;###autoload
(defun org-srs-media-navi-select-start ()
  "Save the current buffer."
  (interactive)
  (save-buffer))

(defvar org-srs-media-navi-select-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'org-srs-media-navi-select-left)
    (define-key map (kbd "<right>") #'org-srs-media-navi-select-right)
    (define-key map (kbd "<up>") #'org-srs-media-navi-select-up)
    (define-key map (kbd "<down>") #'org-srs-media-navi-select-down)
    (define-key map (kbd "<KEYCODE_BUTTON_L1>") #'org-srs-media-navi-select-l1)
    (define-key map (kbd "<KEYCODE_BUTTON_R1>") #'org-srs-media-navi-select-r1)
    (dolist (it '(org-srs-media-navi-select-left
                  org-srs-media-navi-select-right
                  org-srs-media-navi-select-up
                  org-srs-media-navi-select-down
                  org-srs-media-navi-select-l1
                  org-srs-media-navi-select-r1))
      (put it 'repeat-map 'org-srs-media-navi-select-repeat-map))
    map)
  "Keymap to repeat SELECT media adjustments.  Used in `repeat-mode'.")

;;;###autoload
(defun org-srs-media-navi-start ()
  "Toggle review on and off."
  (interactive)
  (if (org-srs-reviewing-p)
      (org-srs-review-quit)
    (org-srs-review-start)))

;;;###autoload
(defun org-srs-media-navi-l1 ()
  "Toggle media playback."
  (interactive)
  (org-srs-media-navi-toggle-playback))

;;;###autoload
(defun org-srs-media-navi-l2 ()
  "Undo only (skip redo entries)."
  (interactive)
  (condition-case nil
      (undo-only)
    (error (org-srs-review-undo))))

;;;###autoload
(defun org-srs-media-navi-r1 ()
  "Read next key and dispatch rating or suspend."
  (interactive)
  (unless (org-srs-media-inside-llm-drawer-p)
    (if-let ((command (org-srs-item-confirm-pending-p)))
        (call-interactively command)
      (org-srs-review-suspend)
      (let ((message-log-max nil))
        (message "Suspend")))))

;;;###autoload
(defun org-srs-media-navi-r2 ()
  "Redo the last undone action."
  (interactive)
  (condition-case nil
      (undo-redo)
    (error (org-srs-review-undo-redo))))

;;;###autoload
(defun org-srs-media-navi-a ()
  "Run org-ctrl-c-ctrl-c at point."
  (interactive)
  (if (org-srs-media-inside-llm-drawer-p)
      (org-ctrl-c-ctrl-c)
    (if-let ((command (org-srs-item-confirm-pending-p)))
        (call-interactively command)
      (org-srs-review-rate-good)
      (let ((message-log-max nil))
        (message "Rated: %s" (propertize "Good" 'face 'success))))))

;;;###autoload
(defun org-srs-media-navi-b ()
  "Quit the current command."
  (interactive)
  (if (org-srs-media-inside-llm-drawer-p)
      (keyboard-quit)
    (if-let ((command (org-srs-item-confirm-pending-p)))
        (call-interactively command)
      (org-srs-review-rate-again)
      (let ((message-log-max nil))
        (message "Rated: %s" (propertize "Again" 'face 'error))))))

;;;###autoload
(defun org-srs-media-navi-x ()
  "Rate the item being reviewed as hard."
  (interactive)
  (unless (org-srs-media-inside-llm-drawer-p)
    (if-let ((command (org-srs-item-confirm-pending-p)))
        (call-interactively command)
      (org-srs-review-rate-hard)
      (let ((message-log-max nil))
        (message "Rated: %s" (propertize "Hard" 'face 'warning))))))

;;;###autoload
(defun org-srs-media-navi-y ()
  "Rate the item being reviewed as easy."
  (interactive)
  (unless (org-srs-media-inside-llm-drawer-p)
    (if-let ((command (org-srs-item-confirm-pending-p)))
        (call-interactively command)
      (org-srs-review-rate-easy)
      (let ((message-log-max nil))
        (message "Rated: %s" (propertize "Easy" 'face 'homoglyph))))))

;;;###autoload
(define-minor-mode org-srs-media-navi-mode
  "Minor mode for navigating org items during media review.
\\{org-srs-media-navi-mode-map}"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<up>") #'org-srs-media-navi-up)
            (define-key map (kbd "<down>") #'org-srs-media-navi-down)
            (define-key map (kbd "<left>") #'org-srs-media-navi-left)
            (define-key map (kbd "<right>") #'org-srs-media-navi-right)
            (define-key map (kbd "<KEYCODE_BUTTON_L1>") #'org-srs-media-navi-l1)
            (define-key map (kbd "<KEYCODE_BUTTON_L2>") #'org-srs-media-navi-l2)
            (define-key map (kbd "<KEYCODE_BUTTON_R1>") #'org-srs-media-navi-r1)
            (define-key map (kbd "<KEYCODE_BUTTON_R2>") #'org-srs-media-navi-r2)
            (define-key map (kbd "<KEYCODE_BUTTON_START>") #'org-srs-media-navi-start)
            (define-key map (kbd "<KEYCODE_BUTTON_A>") #'org-srs-media-navi-a)
            (define-key map (kbd "<KEYCODE_BUTTON_B>") #'org-srs-media-navi-b)
            (define-key map (kbd "<KEYCODE_BUTTON_X>") #'org-srs-media-navi-x)
            (define-key map (kbd "<KEYCODE_BUTTON_Y>") #'org-srs-media-navi-y)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <KEYCODE_BUTTON_A>") #'org-srs-media-navi-select-a)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <KEYCODE_BUTTON_B>") #'org-srs-media-navi-select-b)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <KEYCODE_BUTTON_L1>") #'org-srs-media-navi-select-l1)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <KEYCODE_BUTTON_SELECT>") #'org-srs-media-navi-select-select)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <KEYCODE_BUTTON_START>") #'org-srs-media-navi-select-start)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <KEYCODE_BUTTON_R1>") #'org-srs-media-navi-select-r1)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <left>") #'org-srs-media-navi-select-left)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <right>") #'org-srs-media-navi-select-right)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <up>") #'org-srs-media-navi-select-up)
            (define-key map (kbd "<KEYCODE_BUTTON_SELECT> <down>") #'org-srs-media-navi-select-down)
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
