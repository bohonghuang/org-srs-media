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
(define-minor-mode org-srs-media-navi-mode
  "Minor mode for navigating org items during media review.
\\{org-srs-media-navi-mode-map}"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<up>") #'org-previous-item)
            (define-key map (kbd "<down>") #'org-next-item)
            (define-key map (kbd "<left>") #'org-srs-media-navi-parent-item)
            (define-key map (kbd "<right>") #'org-srs-media-navi-first-child-item)
            (define-key map (kbd "<SPC>") #'org-ctrl-c-ctrl-c)
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
