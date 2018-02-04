;;; cb-flycheck.el --- Flycheck configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(autoload 'evil-define-key "evil-core")
(autoload 'evil-set-initial-state "evil-core")

(require 'spacemacs-keys)

(use-package flycheck
  :ensure t ; load with package.el
  :defer 1
  :commands (global-flycheck-mode
             flycheck-list-errors
             flycheck-error-list-next-error
             flycheck-error-list-previous-error
             flycheck-error-list-goto-error)
  :preface
  (progn
    (autoload 'flycheck-buffer "flycheck")
    (autoload 'flycheck-error-format-message-and-id "flycheck")
    (autoload 'flycheck-get-error-list-window "flycheck")
    (autoload 'flycheck-may-use-echo-area-p "flycheck")
    (autoload 'projectile-project-p "projectile")
    (autoload 'projectile-process-current-project-buffers "projectile")

    (defun cb-flycheck--check-all-project-buffers ()
      (when (and (bound-and-true-p projectile-mode)
                 (projectile-project-p))
        (projectile-process-current-project-buffers
         (lambda (buf)
           (with-current-buffer buf
             (when (bound-and-true-p flycheck-mode)
               (flycheck-buffer)))))))

    (defun cb-flycheck-display-error-messages (errors)
      (unless (flycheck-get-error-list-window 'current-frame)
        (when (and errors (flycheck-may-use-echo-area-p))
          (let ((messages (seq-map #'flycheck-error-format-message-and-id errors)))
            (display-message-or-buffer (string-join messages "\n\n")
                                       flycheck-error-message-buffer
                                       'display-buffer-popup-window)))))

    (defun cb-flycheck-toggle-error-list ()
      "Show or hide the error list."
      (interactive)
      (if-let (window (--first (equal flycheck-error-list-buffer
                                      (buffer-name (window-buffer it)))
                               (window-list)))
          (delete-window window)
        (flycheck-list-errors))))

  :config
  (progn
    (global-flycheck-mode +1)

    (add-hook 'after-save-hook #'cb-flycheck--check-all-project-buffers)

    (setq flycheck-display-errors-function 'cb-flycheck-display-error-messages)
    (setq flycheck-display-errors-delay 0.5)
    (setq flycheck-emacs-lisp-load-path 'inherit)

    (setq flycheck-global-modes
          '(not idris-repl-mode
                dir-locals-mode
                indium-repl-mode
                ;; restclient buffers
                js-mode))

    (spacemacs-keys-set-leader-keys
      "ec" 'flycheck-clear
      "eh" 'flycheck-describe-checker
      "el" 'cb-flycheck-toggle-error-list
      "ee" 'flycheck-explain-error-at-point
      "en" 'flycheck-next-error
      "eN" 'flycheck-previous-error
      "ep" 'flycheck-previous-error
      "es" 'flycheck-select-checker
      "eS" 'flycheck-set-checker-executable
      "ev" 'flycheck-verify-setup)

    (with-eval-after-load 'evil
      (evil-set-initial-state 'flycheck-error-list-mode 'motion)
      (evil-define-key 'motion flycheck-error-list-mode-map
        (kbd "j") #'flycheck-error-list-next-error
        (kbd "k") #'flycheck-error-list-previous-error
        (kbd "RET") #'flycheck-error-list-goto-error
        (kbd "n") #'flycheck-error-list-next-error
        (kbd "p") #'flycheck-error-list-previous-error
        (kbd "q") #'quit-window))

    (bind-key "M-n" 'flycheck-next-error flycheck-mode-map)
    (bind-key "M-p" 'flycheck-previous-error flycheck-mode-map)
    (bind-key "M-j" 'flycheck-next-error flycheck-mode-map)
    (bind-key "M-k" 'flycheck-previous-error flycheck-mode-map)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.2)))))

(provide 'cb-flycheck)

;;; cb-flycheck.el ends here
