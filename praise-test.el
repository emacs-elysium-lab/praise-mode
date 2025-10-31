;;; praise-test.el --- Tests for praise-mode  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

;; Load the file under test from repo root (adjust if you keep tests in ./test).
(load-file (expand-file-name "praise.el" default-directory))

;;;; Helpers

(defun praise-test--fake-blame-output (&optional author summary ts hash)
  "Return a minimal --line-porcelain style fake git blame output."
  (let ((author  (or author  "Alice A. Author"))
        (summary (or summary "Fix: frobnicate the blorg"))
        (ts      (or ts      (number-to-string (truncate (time-to-seconds (current-time))))))
        (hash40  (or hash "0123456789abcdef0123456789abcdef01234567")))
    (mapconcat
     #'identity
     (list
      (format "%s 1 1 1" hash40)     ; header; praise takes first 7 chars
      (format "author %s" author)
      "author-mail <author@example.com>"
      (format "author-time %s" ts)
      "author-tz +0000"
      (format "summary %s" summary))
     "\n")))

(defun praise-test--silence-exit-prompts ()
  "Avoid interactive exit prompts from modified buffers/processes."
  (setq kill-buffer-query-functions nil
        kill-emacs-query-functions nil)
  (set-buffer-modified-p nil))

;;;; Tests

(ert-deftest praise--async-parses-and-calls-callback ()
  "praise--async should parse blame and call back with a formatted string."
  (let* ((fake-out (praise-test--fake-blame-output))
         (cb-called nil)
         (cb-doc nil))
    (with-temp-buffer
      (insert "line 1\nline 2\n")
      (write-region (point-min) (point-max)
                    (setq buffer-file-name (make-temp-file "praise-test-" nil ".el"))
                    nil 'silent)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (_cmd) fake-out))
                ;; Stub async-start to be synchronous and return a live proc.
                ((symbol-function 'async-start)
                 (lambda (fn cb)
                   (let ((res (funcall fn)))
                     (funcall cb res)
                     (make-pipe-process :name "praise-test-proc" :buffer nil)))))
        (praise--async 1 buffer-file-name
                       (lambda ()
                         (setq cb-called t
                               cb-doc praise--last-information))))
      (should cb-called)
      (should (stringp cb-doc))
      (should (string-match-p "Alice A. Author" cb-doc))
      (should (string-match-p "Fix: frobnicate the blorg" cb-doc))
      (should (string-match-p "\\b0123456\\b" cb-doc))
      (praise-test--silence-exit-prompts))))

(ert-deftest praise--eldoc-function-async-then-cached ()
  "First eldoc call returns 'async; second on same line returns cached doc."
  (let ((fake-out (praise-test--fake-blame-output))
        (got-doc nil)
        (returned nil))
    (with-temp-buffer
      (insert "a\nb\n")
      (write-region (point-min) (point-max)
                    (setq buffer-file-name (make-temp-file "praise-test-" nil ".el"))
                    nil 'silent)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (_cmd) fake-out))
                ((symbol-function 'async-start)
                 (lambda (fn cb)
                   (let ((res (funcall fn)))
                     (funcall cb res)
                     (make-pipe-process :name "praise-test-proc2" :buffer nil)))))
        (setq returned
              (praise--eldoc-function
               (lambda (doc &rest _)
                 (setq got-doc doc))))
        (should (eq returned 'async))
        (should (stringp got-doc))
        ;; Same line: should serve cached doc (return nil but still call cb)
        (setq got-doc nil)
        (setq returned
              (praise--eldoc-function
               (lambda (doc &rest _)
                 (setq got-doc doc))))
        (should (null returned))
        (should (stringp got-doc)))
      (praise-test--silence-exit-prompts))))

(ert-deftest praise-mode-adds-and-removes-hook ()
  "Enabling praise-mode adds the eldoc function locally; disabling removes it."
  (with-temp-buffer
    (let ((eldoc-documentation-functions nil))
      (praise-mode 1)
      (unwind-protect
          (progn
            (should (local-variable-p 'eldoc-documentation-functions))
            (should (memq #'praise--eldoc-function eldoc-documentation-functions)))
        (praise-mode -1)
        (should-not (memq #'praise--eldoc-function eldoc-documentation-functions))))
    (praise-test--silence-exit-prompts)))

(ert-deftest praise--async-kills-pending-process ()
  "Starting a new async run kills the previous pending process and installs a new one."
  (let ((fake-out (praise-test--fake-blame-output)))
    (with-temp-buffer
      (insert "x\ny\n")
      (write-region (point-min) (point-max)
                    (setq buffer-file-name (make-temp-file "praise-test-" nil ".el"))
                    nil 'silent)
      (goto-char (point-min))
      ;; Seed an old live process.
      (setq praise--pending-task (make-pipe-process :name "praise-stale" :buffer nil))
      (should (process-live-p praise--pending-task))
      (let ((old-proc praise--pending-task))
        (cl-letf (((symbol-function 'shell-command-to-string)
                   (lambda (_cmd) fake-out))
                  ((symbol-function 'async-start)
                   (lambda (fn cb)
                     (let ((res (funcall fn)))
                       (funcall cb res)
                       (make-pipe-process :name "praise-new" :buffer nil)))))
          (praise--async 1 buffer-file-name (lambda ())))
        ;; Old must be dead; new must be live and named "praise-new".
        (should (not (process-live-p old-proc)))
        (should (processp praise--pending-task))
        (should (process-live-p praise--pending-task))
        (should (string= (process-name praise--pending-task) "praise-new"))))
    (praise-test--silence-exit-prompts)))

(provide 'praise-test)
;;; praise-test.el ends here
