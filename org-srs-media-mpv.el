;;; org-srs-media-mpv.el --- MPVI advices for loop padding and playback -*- lexical-binding: t; -*-

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

;; MPVI advices that adjust loop boundaries and playback behavior
;; for Org-srs media review.

;;; Code:

(require 'cl-lib)

(require 'emms-player-mpv)
(require 'mpvi)

(defcustom org-srs-media-loop-pad 0.25
  "Padding in seconds applied to A-B loop and seek boundaries.
Added to loop-b, subtracted from loop-a and playback-time,
giving context before and after the target subtitle segment."
  :type 'float
  :group 'org-srs-media)

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

(define-advice mpvi-start (:filter-args (args) org-srs-media -50)
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

(provide 'org-srs-media-mpv)
;;; org-srs-media-mpv.el ends here
