;;; config-flycheck.el --- Flycheck configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'config-etags)
  (require 'use-package))

(autoload 'evil-define-key "evil" nil nil 'macro)
(autoload 'evil-set-initial-state "evil")

(require 'dash)
(require 'subr-x)
(require 'spacemacs-keys)

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode-on-safe)

  :commands (flycheck-list-errors
             flycheck-error-list-next-error
             flycheck-error-list-previous-error
             flycheck-error-list-goto-error)

  :bind
  (:map
   flycheck-mode-map
   ("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error)
   ("M-j" . flycheck-next-error)
   ("M-k" . flycheck-previous-error))

  :preface
  (progn
    (autoload 'flycheck-buffer "flycheck")
    (autoload 'flycheck-error-format-message-and-id "flycheck")
    (autoload 'flycheck-get-error-list-window "flycheck")
    (autoload 'flycheck-may-use-echo-area-p "flycheck")
    (autoload 'projectile-project-p "projectile")
    (autoload 'projectile-process-current-project-buffers "projectile")

    (defun config-flycheck-toggle-error-list ()
      "Show or hide the error list."
      (interactive)
      (if-let* ((window (--first (equal flycheck-error-list-buffer
                                        (buffer-name (window-buffer it)))
                                 (window-list))))
          (delete-window window)
        (flycheck-list-errors)))

    (defun config-flycheck--inhibit-if-query-replacing (result)
      (and result (not config-etags-in-query-replace-session-p)))

    (defun config-flycheck--check-all-project-buffers ()
      (unless config-etags-in-query-replace-session-p
        (when (and (bound-and-true-p projectile-mode)
                   (projectile-project-p))
          (projectile-process-current-project-buffers
           (lambda (buf)
             (with-current-buffer buf
               (when (bound-and-true-p flycheck-mode)
                 (flycheck-buffer))))))))

    (defun config-flycheck-display-error-messages (errors)
      (unless (flycheck-get-error-list-window 'current-frame)
        (when (and errors (flycheck-may-use-echo-area-p))
          (let ((messages (seq-map #'flycheck-error-format-message-and-id errors)))
            (display-message-or-buffer (string-join messages "\n\n")
                                       flycheck-error-message-buffer
                                       'display-buffer-popup-window))))))

  :config
  (progn
    (global-flycheck-mode +1)

    (advice-add #'flycheck-may-enable-mode :filter-return #'config-flycheck--inhibit-if-query-replacing)

    (add-hook 'after-save-hook #'config-flycheck--check-all-project-buffers)

    (setq flycheck-display-errors-function 'config-flycheck-display-error-messages)
    (setq flycheck-display-errors-delay 0.5)
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-python-pycompile-executable "python")

    (setq flycheck-global-modes
          '(not idris-repl-mode
                dir-locals-mode
                indium-repl-mode
                ;; restclient buffers
                js-mode))

    (with-eval-after-load 'evil
      (evil-set-initial-state 'flycheck-error-list-mode 'motion)
      (evil-define-key 'motion flycheck-error-list-mode-map
        (kbd "j") #'flycheck-error-list-next-error
        (kbd "k") #'flycheck-error-list-previous-error
        (kbd "RET") #'flycheck-error-list-goto-error
        (kbd "n") #'flycheck-error-list-next-error
        (kbd "p") #'flycheck-error-list-previous-error
        (kbd "q") #'quit-window))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.2)))))


;; flycheck-package is a linter for Elisp package conventions.

(use-package flycheck-package
  :straight t
  :after flycheck
  :preface
  (autoload 'flycheck-package-setup "flycheck-package")
  :config (flycheck-package-setup))

;; Checkdoc is used by flycheck for linting docstrings in elisp.

(use-package checkdoc
  :defer t
  :init
  (progn
    (setq checkdoc-force-docstrings-flag nil)
    (setq checkdoc-arguments-in-order-flag nil)))

(provide 'config-flycheck)

;;; config-flycheck.el ends here
