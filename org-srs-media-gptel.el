;;; org-srs-media-gptel.el --- LLM explanation for media entries -*- lexical-binding: t; -*-

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

;; Use gptel to explain media entries with surrounding subtitle context.

;;; Code:

(require 'cl-lib)

(require 'org)
(require 'org-srs)

(require 'gptel)

(defcustom org-srs-media-explain-system-prompt-file "org-srs-media/system-prompt.org"
  "File path relative to `org-directory' containing the system prompt for gptel."
  :type 'string
  :group 'org-srs-media)

(defun org-srs-media-explain-system-prompt ()
  (with-temp-buffer
    (insert-file (expand-file-name org-srs-media-explain-system-prompt-file org-directory))
    (buffer-string)))

(defun org-srs-media-entry-title ()
  (let ((link (cl-fifth (org-heading-components))))
    (cl-assert (string-match (org-link-make-regexps) link))
    (match-string 3 link)))

(defcustom org-srs-media-explain-drawer-name "LLM"
  "Name of the drawer to store LLM explanations in."
  :type 'string
  :group 'org-srs-media)

(defcustom org-srs-media-explain-context-buffer-name "*Subtitle Context*"
  "Name of the temporary buffer holding subtitle context for gptel."
  :type 'string
  :group 'org-srs-media)

;;;###autoload
(defun org-srs-media-explain-this-entry ()
  (interactive)
  (gptel-abort (current-buffer))
  (org-srs-entry-beginning-of-drawer org-srs-media-explain-drawer-name)
  (goto-char (pos-eol))
  (org-newline-and-indent)
  (let ((context-buffer (get-buffer-create org-srs-media-explain-context-buffer-name))
        (context (cl-loop for offset from -1 to 1
                          nconc (org-with-wide-buffer
                                 (cl-loop initially (org-back-to-heading-or-point-min)
                                          for point = (point)
                                          repeat (if (zerop offset) 1 10)
                                          do (org-forward-heading-same-level offset)
                                          until (and (= (point) point) (not (zerop offset)))
                                          collect (org-srs-media-entry-title))))))
    (with-current-buffer context-buffer
      (delete-region (point-min) (point-max))
      (cl-loop for text in context
               do (insert "- " text) (newline)))
    (let ((gptel-context (cons context-buffer gptel-context))
          (gptel-use-context 'user))
      (gptel-request
          (replace-regexp-in-string (rx "（" (*? anychar) "）") "" (org-srs-media-entry-title))
        :stream t :system (org-srs-media-explain-system-prompt)
        :transforms gptel-prompt-transform-functions))))

(provide 'org-srs-media-gptel)
;;; org-srs-media-gptel.el ends here
