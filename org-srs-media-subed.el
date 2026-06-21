;;; org-srs-media-subed.el --- Convert subtitles to Org-srs flashcards -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bohong Huang

;; Author: Bohong Huang <bohonghuang@qq.com>
;; Maintainer: Bohong Huang <bohonghuang@qq.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (org-srs "1.0") (mpvi) (subed))
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

;; Convert subtitle buffers (subed-mode) into Org-srs media flashcards.

;;; Code:

(require 'cl-lib)
(require 'org-srs)
(require 'mpvi)
(require 'subed)

(defcustom org-srs-media-video-extensions '("mp4" "flv" "mkv" "aac" "mp3" "ogg" "opus")
  "List of media file extensions to search for when converting subtitles."
  :type '(repeat string)
  :group 'org-srs-media)

(defun org-srs-media-convert-subed-buffer (subed-buffer)
  (interactive (list (read-buffer "Subed buffer: ")))
  (cl-flet ((file-name-sans-extension (file)
              (or (progn
                    (string-match (rx bos (group (+? anychar)) "." (+ (not ".")) eos) file)
                    (match-string 1 file))
                  file)))
    (let* ((org-buffer (current-buffer))
           (org-file-directory default-directory)
           (org-level (1+ (with-current-buffer org-buffer (or (org-current-level) 0)))))
      (cl-assert (eq major-mode 'org-mode))
      (with-current-buffer subed-buffer
        (cl-assert (derived-mode-p 'subed-mode))
        (cl-loop with video-file = (cl-loop with subtitle-file = (buffer-file-name)
                                            initially (cl-assert subtitle-file)
                                            for suffix in org-srs-media-video-extensions
                                            for video-file = (concat
                                                              (file-name-sans-extension subtitle-file)
                                                              "." suffix)
                                            when (file-exists-p video-file)
                                            return (file-relative-name video-file org-file-directory)
                                            finally (cl-return (concat (file-name-base subtitle-file) ".mkv")))
                 initially (goto-char (point-min)) (subed-forward-subtitle-text) (subed-backward-subtitle-text)
                 for time-start = (/ (subed-subtitle-msecs-start) 1000.0)
                 for time-stop = (/ (subed-subtitle-msecs-stop) 1000.0)
                 for text-beginning = (progn (subed-jump-to-subtitle-text) (point))
                 for text-end = (progn (subed-jump-to-subtitle-end) (point))
                 for full = (buffer-substring-no-properties text-beginning text-end)
                 do (with-current-buffer org-buffer
                      (org-insert-heading nil t org-level)
                      (insert
                       (org-link-make-string
                        (format "mpv:%s#%f-%f" video-file time-start time-stop)
                        (replace-regexp-in-string (rx (char "\n\r")) " " full))
                       "\n")
                      (org-id-get-create)
                      (org-srs-item-new 'media+front+back)
                      (org-end-of-meta-data t)
                      (goto-char (org-entry-end-position)))
                 while (subed-forward-subtitle-text))))))

(provide 'org-srs-media-subed)
;;; org-srs-media-subed.el ends here
