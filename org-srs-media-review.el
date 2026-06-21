;;; org-srs-media-review.el --- Media card review method for Org-srs -*- lexical-binding: t; -*-

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

;; Custom `org-srs-item-review' method for `media+front+back' cards.

;;; Code:

(require 'cl-lib)

(require 'org)
(require 'org-srs)

(require 'emms-player-mpv)

(defvar org-srs-media-previous-card-new-p nil)

(cl-defmethod org-srs-item-review ((type (eql 'media+front+back)) &rest args)
  (cl-assert (null args))
  (let ((entry-content
         (buffer-substring-no-properties
          (save-excursion
            (org-end-of-meta-data t)
            (point))
          (org-entry-end-position))))
    (org-srs-item-narrow)
    (defvar org-srs-item-new-p)
    (let ((org-srs-item-new-p (org-srs-item-with-current (type . args)
                                (funcall (org-srs-query-predicate-new)))))
      (org-back-to-heading)
      (re-search-forward (org-link-make-regexps) (line-end-position))
      (let ((link-beginning (match-beginning 0)) (link-end (match-end 0)))
        (goto-char link-beginning)
        (org-open-at-point)
        (org-srs-item-card-hide t))
      (let ((callback (org-srs-item-add-hook-once 'org-srs-review-finish-hook #'emms-player-mpv-stop))
            (buffer (current-buffer)))
        (org-srs-item-add-hook-once
         'org-srs-review-continue-hook
         (lambda ()
           (if (org-srs-reviewing-p)
               (with-current-buffer buffer
                 (remove-hook 'org-srs-review-finish-hook #'emms-player-mpv-stop t))
             (emms-player-mpv-stop)))))
      (org-srs-item-add-hook-once
       'org-srs-item-after-confirm-hook
       (org-srs-review-item-hook #'org-srs-item-card-show))
      (setf org-srs-media-previous-card-new-p org-srs-item-new-p)))
  (apply (org-srs-item-confirm) type args))

(provide 'org-srs-media-review)
;;; org-srs-media-review.el ends here
