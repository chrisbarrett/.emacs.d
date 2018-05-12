;;; cb-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-paths)
(require 'noflet)
(require 'spacemacs-keys)
(require 'subr-x)

(use-package ivy
  :straight t
  :commands (ivy-dispatching-done
             ivy-help
             ivy-immediate-done
             ivy-mode
             ivy-partial-or-done
             ivy-resume
             ivy-switch-buffer
             ivy-wgrep-change-to-wgrep-mode)

  :bind
  (
   ;; Browse read-expression histroy with ivy
   :map read-expression-map
   ("C-r" . counsel-expression-history))

  :preface
  (progn
    ;; KLUDGE: Declare dynamic var.
    (defvar org-startup-folded)

    (defun cb-ivy-help ()
      (interactive)
      (let ((org-startup-folded 'nofold))
        (ivy-help)
        (pop-to-buffer (get-buffer "*Ivy Help*"))))

    (defun cb-ivy-with-empty-ivy-extra-directories (f &rest args)
      (let ((ivy-extra-directories nil))
        (apply f args)))

    ;; Define a command for entering wgrep straight from ivy results.

    (autoload 'ivy-occur "ivy")
    (autoload 'ivy-wgrep-change-to-wgrep-mode "ivy")

    (defun ivy-occur-then-wgrep ()
      "Shortcut for calling `ivy-occur' then activating wgrep."
      (interactive)
      (noflet
       ;; HACK: Run the original exit callback, then assume the occur buffer is
       ;; being displayed and change to wgrep.
       ((ivy-exit-with-action
         (action)
         (funcall this-fn (lambda (&rest args)
                            (apply action args)
                            (ivy-wgrep-change-to-wgrep-mode)))))
       (ivy-occur))))

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "r" #'ivy-resume
      "b s" #'ivy-switch-buffer)

    (bind-key "C-c C-r" #'ivy-resume)
    (bind-key "C-x b" #'ivy-switch-buffer))

  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
    (setq ivy-magic-slash-non-match-action nil)

    ;; Do not show extra directories when finding files.
    (setq ivy-extra-directories '("."))
    (advice-add #'counsel-find-file :around #'cb-ivy-with-empty-ivy-extra-directories)

    (define-key ivy-minibuffer-map (kbd "<f1>") #'cb-ivy-help)
    (define-key ivy-occur-mode-map (kbd "C-x C-w") #'ivy-wgrep-change-to-wgrep-mode)
    (define-key ivy-minibuffer-map (kbd "C-z") #'ivy-dispatching-done)
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-partial-or-done)
    (define-key ivy-minibuffer-map (kbd "C-c C-e") #'ivy-occur-then-wgrep)
    (define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-immediate-done)

    ;; Increase the maximum number of candidates that will be sorted
    ;; using `flx'. The default is 200, which means `flx' is almost
    ;; never used. Setting it too high (e.g. 10000) causes lag. This
    ;; seems to be a good compromise (for example, @PythonNut uses it,
    ;; see [1]).
    ;;
    ;; [1]: https://github.com/PythonNut/emacs-config/blob/c8bff5cce293006ec5cdc39a86982431a758a9a0/modules/config-ivy.el#L68
    (setq ivy-flx-limit 2000)

    (ivy-mode))

  :defines (ivy-use-virtual-buffers ivy-count-format))

(use-package flx
  :straight t
  :after ivy)

(use-package ivy-hydra
  :after ivy
  :straight t)

(use-package swiper
  :straight t
  :commands (swiper)
  :init
  (evil-global-set-key 'normal "/" #'swiper))

(use-package counsel
  :straight t
  :demand t

  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function))

  :commands (counsel-descbinds
             counsel-expression-history
             counsel-imenu
             counsel-recentf
             counsel-yank-pop)
  :preface
  (progn
    (autoload 'ivy-immediate-done "ivy")
    (autoload 'counsel-up-directory "counsel")
    (autoload 'counsel-mode "counsel")

    (defun cb-ivy--populate-with-symbol-at-point (f &rest args)
      (if-let ((sym (symbol-at-point)))
          (apply f (symbol-name sym) (cdr args))
        (apply f args))))

  :init
  (spacemacs-keys-set-leader-keys
    "SPC" #'counsel-M-x
    "?"   #'counsel-descbinds
    "f f" #'counsel-find-file
    "f r" #'counsel-recentf
    "k r" #'counsel-yank-pop
    "i"   #'counsel-imenu
    "h d f" #'counsel-describe-function
    "h d v" #'counsel-describe-variable)

  :config
  (progn
    (define-key counsel-find-file-map (kbd "C-M-j") #'ivy-immediate-done)
    (define-key counsel-find-file-map (kbd "C-h") #'counsel-up-directory)

    (defface cb-counsel-separator
      '((t :foreground "gray50"))
      "Face for "
      :group 'cb-ivy)


    (setq counsel-yank-pop-separator (propertize "\n-------------------------------------------\n"
                                                 'face 'cb-counsel-separator))

    ;; Prefill counsel-ag with the symbol at point.
    (advice-add 'counsel-rg :around #'cb-ivy--populate-with-symbol-at-point)
    (advice-add 'counsel-ag :around #'cb-ivy--populate-with-symbol-at-point)

    (counsel-mode +1)))

;; Remembers your choices in completion menus.
(use-package historian
  :straight t
  :demand t
  :config
  (progn
    (setq historian-save-file (f-join cb-emacs-cache-directory "historian"))
    (historian-mode +1)))

;; Uses Historian to sort Ivy candidates by frecency+flx.
(use-package ivy-historian
  :straight t
  :after ivy
  :config
  (progn
    ;; Tweak historian weighting settings. These values are chosen
    ;; subjectively to produce good results.
    (setq ivy-historian-freq-boost-factor 2000)
    (setq ivy-historian-recent-boost 2000)
    (setq ivy-historian-recent-decrement 1000)

    (ivy-historian-mode 1)))

(provide 'cb-ivy)

;;; cb-ivy.el ends here
