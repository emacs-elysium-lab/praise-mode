;;; praise.el --- Show git blame info in eldoc asynchronously -*- lexical-binding: t -*-
;;

;; Author: Ditto <ditto@mf.me>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.1") (async "1.8") (eldoc "1.16"))
;; URL: https://github.com/emacs-elysium-lab/praise-mode
;; Keywords: maint

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides a minor mode that asynchronously displays git blame
;; information in the echo area using eldoc.  It shows commit details for the
;; current line without blocking Emacs.

;; Initially a hack from https://github.com/ISouthRain/emsg-blame, but was
;; completely rewritten to keep it minimal.

;; Usage:
;; M-x praise-mode
;;; Code:

(require 'async)

(defgroup praise nil
  "Minor mode to show git blame in eldoc."
  :group 'tools
  :prefix "praise-")

(defcustom praise-mode-line-ligher ""
  "mode-line ligher"
  :group 'praise
  :type 'string)


(defvar praise--faces
  '((:weight bold)
    (:slant italic :inherit shadow)
    (:inherit default :weight semi-bold)
    (shadow)))

(defvar-local praise--last-line nil)

(defvar-local praise--last-information nil)

(defvar-local praise--pending-task nil)

(defun praise--format-time-sexp (ts)
  "Return a sexp that computes a human-readable relative time for TS (epoch seconds).
Evaluated inside the subprocess."
  `(when-let* ((ts ,ts)
               (units '((year  . 31536000) ; 365 * 24 * 60 * 60
                        (month . 2592000)  ; 30 * 24 * 60 * 60
                        (day   . 86400)
                        (hour  . 3600)
                        (min   . 60)))
               (time (string-to-number (string-trim ts)))
               (diff (- (time-to-seconds (current-time)) time)))
     (cond
       ((< diff 60) "just now")
       (t
        (or (cl-loop for (name . secs) in units
                  for n = (floor (/ diff secs))
                  when (>= n 1)
                  return (format "%d %s ago"
                                 n
                                 (if (= n 1)
                                     (symbol-name name)
                                   (concat (symbol-name name) "s"))))
            ;; fallback
            (format "%s ago" ts))))))


(defun praise--async (line-num file &optional cb)
  (let ((orig-buffer (current-buffer))
        (dir (file-name-directory (expand-file-name file)))
        (fname (file-local-name (expand-file-name file))))
    (when (and praise--pending-task
               (processp praise--pending-task)
               (process-live-p praise--pending-task))
      (delete-process praise--pending-task))
    (setq praise--pending-task
          (async-start
           `(lambda ()
              (require 'cl-lib)
              (let* ((default-directory ,dir)
                     (cmd (format "git blame -L %d,%d --line-porcelain %s"
                                  ,line-num ,line-num (shell-quote-argument ,fname)))
                     (out (shell-command-to-string cmd))
                     (ls  (and out (split-string out "\n" t)))
                     (head (car ls))
                     (hash (and head (substring head 0 (min 7 (length head)))))
                     (author-line  (cl-find-if (lambda (s) (string-prefix-p "author " s)) ls))
                     (time-line    (cl-find-if (lambda (s) (string-prefix-p "author-time " s)) ls))
                     (summary-line (cl-find-if (lambda (s) (string-prefix-p "summary " s)) ls))
                     (author  (and author-line  (substring author-line  (length "author "))))
                     (ts-str  (and time-line    (substring time-line    (length "author-time "))))
                     (summary (and summary-line (substring summary-line (length "summary "))))
                     (reltime ,(praise--format-time-sexp 'ts-str)))
                (list (or author "unknown")
                      (or reltime "unknown")
                      (or summary " ")
                      (or hash "unknown"))))
           (lambda (result)
             (let ((formatted (mapconcat
                               (lambda (pair)
                                 (propertize (car pair) 'face (cdr pair)))
                               ;; the proportized string cannot be serialized between subprocesses.
                               (cl-mapcar #'cons
                                          result
                                          praise--faces)
                               " | ")))
               ;; cb in original buffer
               (with-current-buffer orig-buffer
                 (setq praise--last-information formatted)
                 (when cb (funcall cb)))))))))


(defun praise--eldoc-function (cb)
  "ElDoc documentation function for git blame information at current line."
  (let ((current-line (line-number-at-pos))
        (file (buffer-file-name)))
    (if (and file (not (equal current-line praise--last-line)))
        (progn
          (setq praise--last-line current-line)
          (praise--async current-line file
                      (lambda () (funcall cb praise--last-information
                                          :thing 'BLAME :face 'default)))
          'async)
      (when praise--last-information
        (funcall cb praise--last-information :thing 'BLAME :face 'default)
        nil))))







;;;###autoload
(define-minor-mode praise-mode
    "Minor mode for showing git blame message in ElDoc."
  :lighter praise-mode-line-ligher
  (if praise-mode
      ;; put it at very end of the `eldoc-documentation-functions'
      (add-hook 'eldoc-documentation-functions #'praise--eldoc-function 100 t)
    (remove-hook 'eldoc-documentation-functions #'praise--eldoc-function)))

(provide 'praise)

;;; praise.el ends here
