;;; org-srs-media.el --- Media (video/audio) integration for org-srs -*- lexical-binding: t; -*-

;; Author: Coco
;; Package-Requires: ((emacs "27.1") (org-srs) (mpvi) (subed) (emms) (cl-lib))
;; Keywords: multimedia, srs, org

;;; Commentary:

;; This package provides media (video/audio) integration for org-srs.
;; It includes:
;; - Subtitle-to-flashcard conversion (org-srs-video-convert-subed-buffer)
;; - Custom review method for media+front+back card type
;; - MPV advices for loop padding and playback control
;; - LLM-based entry explanation (via gptel)

;;; Code:

(require 'cl-lib)
(require 'org-srs)
(require 'mpvi)
(require 'subed)

(defun org-srs-media-convert-subed-buffer (subed-buffer)
  (interactive (list (read-buffer "Subed buffer: ")))
  (cl-flet ((file-name-sans-extension (file)
              (or (progn
                    (string-match (rx bos (group (+? anychar)) "." (+ (not ".")) eos) file)
                    (match-string 1 file))
                  file)))
    (let* ((org-buffer (current-buffer))
           ;; (org-file (buffer-file-name))
           (org-file-directory default-directory)
           (org-level (1+ (with-current-buffer org-buffer (or (org-current-level) 0)))))
      (cl-assert (eq major-mode 'org-mode))
      (with-current-buffer subed-buffer
        (cl-assert (derived-mode-p 'subed-mode))
        (cl-loop with video-file = (cl-loop with subtitle-file = (buffer-file-name)
                                            initially (cl-assert subtitle-file)
                                            for suffix in '("mp4" "flv" "mkv" "aac" "mp3" "ogg" "opus")
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
                 ;; for text-line-end = (progn (end-of-line) (point))
                 for text-end = (progn (subed-jump-to-subtitle-end) (point))
                 ;; for front = (buffer-substring-no-properties text-beginning text-line-end)
                 ;; for back = (buffer-substring-no-properties (1+ text-line-end) text-end)
                 for full = (buffer-substring-no-properties text-beginning text-end)
                 do (with-current-buffer org-buffer
                      (org-insert-heading nil t org-level)
                      (insert
                       (org-link-make-string
                        (format "mpv:%s#%f-%f" video-file time-start time-stop)
                        ;; (format "▶ %s → %s"
                        ;;         (mpvi-secs-to-hms time-start nil t)
                        ;;         (mpvi-secs-to-hms time-stop nil t))
                        (replace-regexp-in-string (rx (char "\n\r")) " " full))
                       "\n")
                      (org-id-get-create)
                      (org-srs-item-new 'media+front+back)
                      (org-end-of-meta-data t)
                      (goto-char (org-entry-end-position)))
                 while (subed-forward-subtitle-text))))))

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
      (org-srs-item-add-hook-once
       'org-srs-review-continue-hook
       (lambda ()
         (unless (org-srs-reviewing-p)
           (emms-player-mpv-stop)))
       75)
      (org-srs-item-add-hook-once
       'org-srs-item-after-confirm-hook
       #'org-srs-item-card-show)
      (setf org-srs-media-previous-card-new-p org-srs-item-new-p)))
  (apply (org-srs-item-confirm) type args))

(defun org-srs-media-explain-system-prompt ()
  (with-temp-buffer
    (insert-file (expand-file-name "org-mode/提示词/日语句子.org" org-directory))
    (buffer-string)))

(defun org-srs-media-entry-title ()
  (let ((link (cl-fifth (org-heading-components))))
    (cl-assert (string-match (org-link-make-regexps) link))
    (match-string 3 link)))

(defun org-srs-media-explain-this-entry ()
  (interactive)
  (require 'gptel)
  (require 'gptel-request)
  (org-srs-entry-beginning-of-drawer "LLM")
  (goto-char (pos-eol))
  (org-newline-and-indent)
  (let ((context-buffer (get-buffer-create " 字幕上下文"))
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
               do (insert text) (newline)))
    (gptel-request
        (replace-regexp-in-string (rx "（" (*? anychar) "）") "" (org-srs-media-entry-title))
      :stream t :system (org-srs-media-explain-system-prompt)
      :context (cons context-buffer gptel-context))))

(defvar org-srs-media-loop-pad 0.25)

(define-advice mpvi-set (:around (fun name &rest args) org-srs-media)
  (cl-case name
    ((ab-loop-a ab-loop-b)
     (if (and args (not (equal args '("no"))))
         (cl-destructuring-bind (time) args
           (funcall fun name (max (+ time (cl-ecase name
                                            (ab-loop-a (- org-srs-media-loop-pad))
                                            (ab-loop-b (+ org-srs-media-loop-pad))))
                                  0.0)))
       (apply fun name args)))
    (playback-time
     (defvar org-srs-item-new-p)
     (cond
      ((not (boundp 'org-srs-item-new-p)) (apply fun name args))
      ((null args) (apply fun name args))
      ((not org-srs-item-new-p)
       (cl-destructuring-bind (time) args
         (funcall fun name (max (- time org-srs-media-loop-pad) 0.0))))))
    (t (apply fun name args))))

(define-advice mpvi-cmd (:around (fun args) org-srs-media)
  (cl-case (car args)
    (seek
     (defvar org-srs-item-new-p)
     (cond
      ((not (boundp 'org-srs-item-new-p)) (funcall fun args))
      ((null args) (funcall fun args))
      ((or (not org-srs-item-new-p) (not org-srs-media-previous-card-new-p))
       (cl-destructuring-bind (time &rest rest) (cdr args)
         (funcall fun `(seek ,(max (- time org-srs-media-loop-pad) 0.0) . ,rest))))))
    (t (funcall fun args))))

(define-advice mpvi-start (:filter-args (args) org-srs-media)
  (cl-destructuring-bind (path &rest args) args
    (cons path (if (numberp (car args)) (cons nil args) args))))

(define-advice mpvi-org-link-push (:around (fun link) org-srs-media)
  (cl-destructuring-bind (file beg end) (mpvi-parse-link link)
    (if (and (emms-player-mpv-proc-playing-p)
             (process-live-p emms-player-mpv-proc)
             (not (mpvi-get 'pause))
             (<= beg (mpvi-get 'time-pos) end)
             (file-equal-p file (mpvi-get 'path))
             (not (boundp 'org-srs-item-new-p)))
        (mpvi-pause)
      (funcall fun link))))

(provide 'org-srs-media)
;;; org-srs-media.el ends here
