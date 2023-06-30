;;; cb-completion.el --- Configuration for completion engines.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
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



(use-package simple

  ;; M-n beyond the end in `completing-read' will use the thing at point
  :preface
  (autoload 'ffap-guesser "ffap")
  (autoload 'thing-at-point-url-at-point "thingatpt")
  (defun cb-minibuffer-default-add-function ()
    (with-selected-window (minibuffer-selected-window)
      (delete-dups
       (delq nil
             (list (thing-at-point 'symbol)
                   (thing-at-point 'list)
                   (ffap-guesser)
                   (thing-at-point-url-at-point))))))
  :custom
  (minibuffer-default-add-function #'cb-minibuffer-default-add-function)

  :custom
  ;; Hide commands irrelevant to current mode from M-x
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package crm
  ;; Show additional information in prompt if completing-read-multiple is used
  :preface
  (define-advice completing-read-multiple (:filter-args (args) show-indicator)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args))))

(provide 'cb-completion)

;;; cb-completion.el ends here
