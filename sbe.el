;;; sbe.el --- Show git blame info in eldoc asynchronously -*- lexical-binding: t -*-
;;

;; Author: Ditto <ditto@mf.me>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.1") (async "1.8") (eldoc "1.16"))
;; Keywords: git
;; URL: https://github.com/emacs-elysium-lab/show-blame-eldoc-mode

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
;; M-x show-blame-eldoc-mode
;;; Code:

(require 'async)

(defgroup sbe nil
  "Minor mode to show git blame in eldoc."
  :group 'tools
  :prefix "sbe-")

(defcustom sbe-mode-line-ligher ""
  "mode-line ligher"
  :group 'sbe
  :type 'string)


(defvar sbe--faces
  '((:weight bold)
    (:slant italic :inherit shadow)
    (:inherit default :weight semi-bold)
    (shadow)))

(defvar-local sbe--last-line nil)

(defvar-local sbe--last-information nil)

(defvar-local sbe--pending-task nil)

(defun sbe--format-time-sexp (ts)
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


(defun sbe--async (line-num file &optional cb)
  (let ((orig-buffer (current-buffer))
        (dir (file-name-directory (expand-file-name file)))
        (fname (file-local-name (expand-file-name file))))
    (when (and sbe--pending-task
               (processp sbe--pending-task)
               (process-live-p sbe--pending-task))
      (delete-process sbe--pending-task))
    (setq sbe--pending-task
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
                     (reltime ,(sbe--format-time-sexp 'ts-str)))
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
                                          sbe--faces)
                               " | ")))
               ;; cb in original buffer
               (with-current-buffer orig-buffer
                 (setq sbe--last-information formatted)
                 (when cb (funcall cb)))))))))


(defun sbe--eldoc-function (cb)
  "ElDoc documentation function for git blame information at current line."
  (let ((current-line (line-number-at-pos))
        (file (buffer-file-name)))
    (if (and file (not (equal current-line sbe--last-line)))
        (progn
          (setq sbe--last-line current-line)
          (sbe--async current-line file
                      (lambda () (funcall cb sbe--last-information
                                          :thing 'BLAME :face 'default)))
          'async)
      (when sbe--last-information
        (funcall cb sbe--last-information :thing 'BLAME :face 'default)
        nil))))







;;;###autoload
(define-minor-mode sbe-mode
    "Minor mode for showing git blame message in ElDoc."
  :lighter sbe-mode-line-ligher
  (if sbe-mode
      ;; put it at very end of the `eldoc-documentation-functions'
      (add-hook 'eldoc-documentation-functions #'sbe--eldoc-function 100 t)
    (remove-hook 'eldoc-documentation-functions #'sbe--eldoc-function)))

(provide 'sbe)

;;; sbe.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("sbe" . "show-blame-eldoc"))
;; End:
