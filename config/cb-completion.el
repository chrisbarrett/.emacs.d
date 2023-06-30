;;; cb-completion.el --- Configuration for completion engines.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'no-littering))

(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-max-saved-items 100)
  (recentf-exclude (list (rx "TAGS" eos)
                         (rx ".DS_Store" eos)
                         (rx "." (or "gz" "zip" "tar" "7z") eos)
                         (rx bos "/sudo:root@")
                         (rx "/.git/")
                         (rx "/" (or "build" "dist" "target") "/")
                         (rx (or "/var/folders/"
                                 "/usr/local/Cellar/"
                                 "/tmp/"
                                 "/nix/store/"))
                         'file-remote-p
                         (regexp-quote no-littering-etc-directory)
                         (regexp-quote no-littering-var-directory))))



;; Increase minibuffer history length.

(setq history-length 1000)

;; Hide boring files from find-file completion candidates.

(define-advice completion--file-name-table (:filter-return (result) remove-boring-files)
  (if (and (listp result) (stringp (car result)) (cdr result))
      (let ((matches-boring (rx-to-string `(and (or "." ".." ".DS_Store" ,@completion-ignored-extensions) eos))))
        (seq-remove (lambda (it)
                      (and (stringp it) (string-match-p matches-boring it)))
                    result))
    result))

;; Remove any lingering *completions* buffer on minibuffer exit
(defun cb--cleanup-completions-buffer ()
  (when-let* ((buf (get-buffer "*Completions*")))
    (kill-buffer buf)))

(add-hook 'minibuffer-exit-hook #'cb--cleanup-completions-buffer)

(provide 'cb-completion)

;;; cb-completion.el ends here
