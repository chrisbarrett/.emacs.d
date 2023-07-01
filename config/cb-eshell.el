;;; cb-eshell --- Configuration for builtin shell.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :preface
  (defun cb-eshell-skip-prompt ()
    (goto-char (line-beginning-position))
    (skip-chars-forward "#$λ "))

  (defun cb-eshell-prompt ()
    (concat (propertize (abbreviate-file-name (eshell/pwd))
                        'face 'font-lock-comment-face)
            "\n"
            (if (= (user-uid) 0) " # " " λ ")))

  :custom
  (eshell-prefer-lisp-functions t)
  (eshell-prefer-lisp-variables t)
  (eshell-prompt-function 'cb-eshell-prompt)
  (eshell-skip-prompt-function 'cb-eshell-skip-prompt)
  (eshell-prompt-regexp (rx bol (* (not (any "#$\n"))) space (any "#$λ") space))

  :general
  (:states 'insert :keymaps 'eshell-mode-map
    [remap indent-for-tab-command] 'completion-at-point
    "C-n" 'eshell-next-matching-input-from-input
    "C-p" 'eshell-previous-matching-input-from-input
    "C-a" 'eshell-bol
    "C-e" 'end-of-line))



;;; Custom commands

(require 'cl-lib)

(cl-eval-when (compile)
  (require 'project)
  (require 'eshell)
  (require 'em-dirs))

(defun eshell/j (&rest query)
  "Jump to a directory with fasd QUERY."
  (let* ((command `("fasd" "-ld" ,@(mapcar #'shell-quote-argument query)))
         (output (shell-command-to-string (string-join command " ")))
         (matches (nreverse (split-string output "\n" t))))
    (if-let* ((dir (car matches)))
        (eshell/cd dir)
      (let ((message-log-max))
        (message "No fasd match")))))

(defun eshell/g ()
  "Navigate to the Git root."
  (let (message-log-max)
    (if-let* ((dir (locate-dominating-file default-directory ".git")))
        (progn
          (message "Moving to git repository root")
          (eshell/cd dir))
      (if-let* ((dir (project-root)))
          (progn
            (message "Moving to project root")
            (eshell/cd dir))
        (user-error "Not in a project or git repo")))))

(provide 'cb-eshell)

;;; cb-eshell.el ends here
