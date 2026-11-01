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
;; New cards play with audio and reveal text after one confirmation.
;; Reviewed cards start muted; the first confirmation enables audio, and
;; the second reveals heading/body and makes the card ready for rating.
;; Mute changes use `mpvi-toggle-mute' after checking the current state.
;; A cold player starts with `mpvi-mpv-mute-p' and must become ready
;; within five seconds before confirmations are accepted.

;;; Code:

(require 'cl-lib)

(require 'org)
(require 'org-srs)

(require 'org-srs-media-mpv)

(defvar org-srs-media-previous-card-new-p nil)

(defun org-srs-media-review--stop ()
  "Stop media playback when its review session finishes."
  (emms-player-mpv-stop)
  (remove-hook 'org-srs-review-finish-hook #'org-srs-media-review--stop))

(cl-defmethod org-srs-item-review ((type (eql 'media+front+back)) &rest args)
  (cl-assert (null args))
  (defvar org-srs-reviewing-p)
  (org-srs-item-narrow)
  (let* ((newp (org-srs-item-with-current (type . args)
                (funcall (org-srs-query-predicate-new))))
         (active t)
         completed pending-hook cleanup)
    (cl-labels ((set-mute (value)
                  (setq mpvi-mpv-mute-p value)
                  (when (and (emms-player-mpv-proc-playing-p)
                             (not (eq (mpvi-get 'mute) value)))
                    (mpvi-toggle-mute))))
      ;; The startup preference handles a new player; toggle handles an
      ;; already-playing video before opening/seeking the next card.
      (set-mute (not newp))
      (add-hook 'org-srs-review-finish-hook #'org-srs-media-review--stop)
      (setq cleanup
            (org-srs-item-add-hook-once
             'org-srs-review-continue-hook
             (lambda ()
               (setq active nil)
               (set-mute nil)
               (org-srs-item-card-show)
               (unless (org-srs-reviewing-p)
                 (org-srs-media-review--stop)))
             -90))
      (unwind-protect
          (progn
            (org-back-to-heading)
            (re-search-forward (org-link-make-regexps) (line-end-position))
            (goto-char (match-beginning 0))
            (defvar org-srs-item-new-p)
            (let ((org-srs-item-new-p newp))
              (org-open-at-point))
            (org-srs-item-card-hide t)
            ;; Toggle requires live playback, so finish cold startup before
            ;; accepting confirmations instead of maintaining async state.
            (with-timeout (5 (user-error "Media playback did not start"))
              (while (not (emms-player-mpv-proc-playing-p))
                (accept-process-output nil 0.05)))
            (setq org-srs-media-previous-card-new-p newp)
            (setq pending-hook
                  (org-srs-item-add-hook-once
                   'org-srs-item-after-confirm-hook
                   (org-srs-review-item-hook
                    (lambda ()
                      (if (and (not newp) active (org-srs-reviewing-p))
                          (progn
                            (set-mute nil)
                            (setq pending-hook
                                  (org-srs-item-add-hook-once
                                   'org-srs-item-after-confirm-hook
                                   (org-srs-review-item-hook #'org-srs-item-card-show)
                                   -10))
                            (apply (org-srs-item-confirm) type args))
                        (org-srs-item-card-show))))
                   -10))
            (apply (org-srs-item-confirm) type args)
            (setq completed t))
        (unless completed
          (when pending-hook
            (remove-hook 'org-srs-item-after-confirm-hook pending-hook t))
          (let ((org-srs-reviewing-p nil))
            (funcall cleanup)))))))

(provide 'org-srs-media-review)
;;; org-srs-media-review.el ends here
