;;; config-elisp.el --- Configuration for Emacs Lisp.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'straight)
(require 'subr-x)


;; Define some modes for specific files.

(define-derived-mode dir-locals-mode emacs-lisp-mode "dir-locals")
(add-to-list 'auto-mode-alist '("\\.dir-locals.el\\'" . dir-locals-mode))

(define-derived-mode caskfile-mode emacs-lisp-mode "caskfile")
(add-to-list 'auto-mode-alist '("/Cask\\'" . caskfile-mode))


;; Define an eval-buffer command for elisp that executes within a straight
;; transaction.

(defun config-elisp-eval-buffer ()
  "Evaluate the current buffer as Elisp code, within a straight transaction."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (when (and (buffer-file-name) (string= (buffer-file-name) user-init-file))
      (straight-mark-transaction-as-init))
    (eval-buffer))
  (message "Evaluating %s... done." (buffer-name)))


;; IELM is the Elisp repl built in to Emacs.

(use-package ielm
  :general (:keymaps 'emacs-lisp-mode-map "C-c C-z" #'ielm)
  :preface
  (defun config-elisp-pop-to-elisp-buffer ()
    (interactive)
    (if-let* ((buf (seq-find (lambda (buf)
                               (with-current-buffer buf
                                 (derived-mode-p 'emacs-lisp-mode)))
                             (buffer-list))))
        (pop-to-buffer buf)
      (user-error "No Emacs Lisp buffers")))
  :config
  (progn
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*ielm*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))
    (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-z") #'config-elisp-pop-to-elisp-buffer)))

;; elisp-slime-nav provides a command for navigating to the definitions of
;; things in elisp in a uniform way.

(use-package elisp-slime-nav
  :straight t
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  :general
  (:keymaps 'emacs-lisp-mode-map :states 'normal
   "M-." #'elisp-slime-nav-find-elisp-thing-at-point))

;; eldoc shows function parameters in the minibuffer.

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2))

;; nameless hides the package prefix in elisp buffers, which can make things
;; easier to read.

(use-package nameless
  :straight t
  :defines (nameless-current-name)
  :commands nameless-mode
  :hook (emacs-lisp-mode . nameless-mode)
  :config
  (progn
    (add-to-list 'display-buffer-alist
                 `(,(rx "*Ilist*")
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . right)
                   (slot            . 1)
                   (window-width   . 0.25)))

    (setq nameless-prefix ":")
    (setq nameless-private-prefix t))

  ;; Teach nameless mode how to insinuate itself into imenu-list.
  :init
  (add-hook 'imenu-list-update-hook #'config-elisp--configure-imenu-list)
  :preface
  (progn
    (defvar nameless-current-name nil)

    (defun config-elisp--configure-imenu-list ()
      (when (bound-and-true-p imenu-list-buffer-name)
        (with-current-buffer imenu-list-buffer-name
          (if-let* ((nameless-current-name (config-elisp--nameless-name-for-ilist-buffer)))
              (nameless-mode +1)
            (nameless-mode -1)))))

    (defun config-elisp--nameless-name-for-ilist-buffer ()
      (when (bound-and-true-p imenu-list--displayed-buffer)
        (with-current-buffer imenu-list--displayed-buffer
          (when (derived-mode-p 'emacs-lisp-mode)
            nameless-current-name))))))

;; debug.el defines the emacs lisp debugger.

(use-package debug
  :defer t
  :general (:keymaps 'debugger-mode-map
            "," 'major-mode-hydra--debugger-mode/body
            "j" 'forward-to-indentation
            "k" 'backward-to-indentation)
  :init
  (add-hook 'debugger-mode-hook #'hl-line-mode))

;; helpful is a more feature-rich alternative to the Emacs Lisp help buffer.

(use-package helpful
  :straight t
  :general
  (:keymaps '(emacs-lisp-mode-map helpful-mode-map) :states '(motion normal)
   "K" 'helpful-at-point)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helpful ")
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (slot            . 1)
                 (window-width    . 0.5))))

(provide 'config-elisp)

;;; config-elisp.el ends here
