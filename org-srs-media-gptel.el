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

(cl-defun org-srs-media-drawer-folded-p (&optional (element (org-element-at-point)))
  (when-let ((post (org-element-post-affiliated element)))
    (let ((start (save-excursion
                   (goto-char post)
                   (line-end-position))))
      (org-fold-folded-p start 'drawer))))

(cl-defun org-srs-media-inside-llm-drawer-p (&optional (position (point)))
  (save-excursion
    (org-back-to-heading-or-point-min)
    (let ((heading-start (point)))
      (org-srs-entry-end-of-meta-data t)
      (when (re-search-backward (rx bol (* blank) ":" (literal org-srs-media-explain-drawer-name) ":" (* blank) eol) heading-start t)
        (let ((element (org-element-at-point)))
          (cl-assert (eq (org-element-type element) 'drawer))
          (or (not (org-srs-media-drawer-folded-p element))
              (< (org-element-begin element) position (org-element-end element))))))))

(defconst org-srs-media-gptel-detect-language--kana-ranges
  '((#x3040 . #x309F)   ; Hiragana
    (#x30A0 . #x30FF)   ; Katakana
    (#x31F0 . #x31FF)   ; Katakana Phonetic Extensions
    (#xFF66 . #xFF9F))  ; Halfwidth Katakana
  "Character ranges that are unambiguously Japanese.")

(defconst org-srs-media-gptel-detect-language--kanji-ranges
  '((#x3400  . #x4DBF)  ; CJK Unified Ideographs Extension A
    (#x4E00  . #x9FFF)  ; CJK Unified Ideographs
    (#xF900  . #xFAFF)  ; CJK Compatibility Ideographs
    (#x20000 . #x2A6DF) ; CJK Extension B
    (#x2A700 . #x2EBEF)) ; CJK Extensions C/D/E/F
  "Character ranges for han/kanji, shared between Japanese and Chinese.")

(defun org-srs-media-gptel-detect-language--char-in-ranges-p (char ranges)
  "Return non-nil if CHAR is inside any (START . END) cons in RANGES."
  (let ((found nil))
    (while (and ranges (not found))
      (let ((range (car ranges)))
        (when (and (>= char (car range)) (<= char (cdr range)))
          (setq found t)))
      (setq ranges (cdr ranges)))
    found))

(defun org-srs-media-gptel-detect-language (string)
  "Detect the language of STRING, returning `:en' or `:ja'.

Japanese is detected by the presence of kana (unambiguous) or any kanji
(never present in English).  Anything else -- Latin text, digits, the
empty string -- is reported as `:en'.

  (org-srs-media-gptel-detect-language \"hello world\")      ; => :en
  (org-srs-media-gptel-detect-language \"こんにちは\")         ; => :ja
  (org-srs-media-gptel-detect-language \"日本語\")             ; => :ja
  (org-srs-media-gptel-detect-language \"Hello 世界\")        ; => :ja
  (org-srs-media-gptel-detect-language \"\")                 ; => :en"
  (let ((kana 0)
        (kanji 0)
        (len (length string))
        (i 0))
    (while (< i len)
      (let ((ch (aref string i)))
        (cond
         ((or (< ch #x41)                 ; control, space, ASCII punctuation
              (and (> ch #x5A) (< ch #x61)) ; [ \ ] ^ _ `
              (and (> ch #x7A) (< ch #x80)) ; { | } ~ DEL
              (and (> ch #x7F) (< ch #xC0))) ; C1 controls / Latin-1 punct
          nil)                            ; not a signal, skip
         ((org-srs-media-gptel-detect-language--char-in-ranges-p ch org-srs-media-gptel-detect-language--kana-ranges)
          (setq kana (1+ kana)))
         ((org-srs-media-gptel-detect-language--char-in-ranges-p ch org-srs-media-gptel-detect-language--kanji-ranges)
          (setq kanji (1+ kanji)))))
      (setq i (1+ i)))
    (if (or (> kana 0) (> kanji 0)) 'ja 'en)))

;;;###autoload
(defun org-srs-media-explain-this-entry ()
  (interactive)
  (gptel-abort (current-buffer))
  (org-srs-entry-beginning-of-drawer org-srs-media-explain-drawer-name)
  (beginning-of-line)
  (let ((element (org-element-at-point)))
    (delete-region (max (point-min) (org-element-begin element)) (min (org-element-end element) (point-max))))
  (org-srs-entry-beginning-of-drawer org-srs-media-explain-drawer-name)
  (end-of-line)
  (org-newline-and-indent)
  (let ((context-buffer (get-buffer-create org-srs-media-explain-context-buffer-name))
        (context (cl-loop for offset from -1 to 1
                          nconc (org-with-wide-buffer
                                 (cl-loop initially (org-back-to-heading-or-point-min)
                                          for point = (point)
                                          repeat (if (zerop offset) 1 10)
                                          do (org-forward-heading-same-level offset)
                                          until (and (= (point) point) (not (zerop offset)))
                                          collect (org-srs-media-entry-title)))))
        (title (org-srs-media-entry-title)))
    (with-current-buffer context-buffer
      (delete-region (point-min) (point-max))
      (cl-loop for text in context
               do (insert "- " text) (newline)))
    (let ((gptel-context (cons context-buffer gptel-context))
          (gptel-use-context 'user))
      (gptel-request
          (replace-regexp-in-string (rx "（" (*? anychar) "）") "" title)
        :stream t :system (org-srs-media-explain-system-prompt (org-srs-media-gptel-detect-language title))
        :transforms gptel-prompt-transform-functions))))

(provide 'org-srs-media-gptel)
;;; org-srs-media-gptel.el ends here
