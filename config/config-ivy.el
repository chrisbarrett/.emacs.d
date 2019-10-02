;;; config-ivy.el --- Configuration for Ivy.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(cl-eval-when (compile)
  (require 'org))

;; ivy is a package that provides incremental completion, similar to helm or
;; ido, but actively maintained.

(use-package ivy
  :straight t
  :commands (ivy-occur ivy-help ivy-mode)
  :general
  ("C-c C-r" #'ivy-resume
   "C-x b" #'ivy-switch-buffer)

  (:keymaps 'ivy-occur-mode-map
   "C-x C-w" #'ivy-wgrep-change-to-wgrep-mode)

  (:keymaps 'ivy-minibuffer-map
   "C-z" #'ivy-dispatching-done
   "C-l" #'ivy-partial-or-done
   "C-<return>" #'ivy-immediate-done
   "M-<return>" #'ivy-immediate-done
   "C-SPC" #'config-ivy-eshell-action)

  ;; Browse read-expression histroy with ivy
  (:keymaps 'read-expression-map
   "C-r" #'counsel-expression-history)

  :init
  (progn
    (declare-function ivy-completing-read "ivy")
    (setq completing-read-function #'ivy-completing-read))

  :preface
  (progn

    (defun config-ivy-help ()
      (interactive)
      (let ((org-startup-folded 'nofold))
        (ivy-help)
        (pop-to-buffer (get-buffer "*Ivy Help*"))))

    (defun config-ivy-with-empty-ivy-extra-directories (f &rest args)
      (let ((ivy-extra-directories nil))
        (apply f args)))

    ;; Define a command to pop open eshell for the candidate.

    (defun config-ivy-eshell-action ()
      "Open eshell at the target."
      (interactive)
      (ivy-exit-with-action
       (lambda (candidate)
         (let ((default-directory
                 (cond
                  ((f-dir-p candidate) candidate)
                  ((f-file-p candidate) (f-dirname candidate))
                  (t
                   (user-error "No available eshell action for candidate")))))
           (eshell t)))))

    ;; Define a command for entering wgrep straight from ivy results.

    (defun config-ivy-occur-then-wgrep ()
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

  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
    (setq ivy-magic-slash-non-match-action nil)
    (setq ivy-height 20)

    ;; Do not show extra directories when finding files.
    (setq ivy-extra-directories '("."))
    (advice-add #'counsel-find-file :around #'config-ivy-with-empty-ivy-extra-directories)

    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "<f1>") #'config-ivy-help)
    (define-key ivy-minibuffer-map (kbd "C-c C-e") #'config-ivy-occur-then-wgrep)

    ;; Increase the maximum number of candidates that will be sorted
    ;; using `flx'. The default is 200, which means `flx' is almost
    ;; never used. Setting it too high (e.g. 10000) causes lag. This
    ;; seems to be a good compromise (for example, @PythonNut uses it,
    ;; see [1]).
    ;;
    ;; [1]: https://github.com/PythonNut/emacs-config/blob/c8bff5cce293006ec5cdc39a86982431a758a9a0/modules/config-ivy.el#L68
    (setq ivy-flx-limit 2000)

    (ivy-mode)))

;; ivy-posframe shows ivy in a child frame.

(use-package ivy-posframe
  :straight t
  :hook (ivy-mode . ivy-posframe-enable)
  :config
  (general-setq ivy-posframe-border-width 20
                ivy-posframe-style 'frame-center
                ivy-posframe-hide-minibuffer t
                ivy-posframe-parameters '((alpha 100 100))))

;; `ivy-filthy-rich' shows extra information in ivy buffers.

(use-package ivy-filthy-rich
  :straight (:host github :repo "casouri/ivy-filthy-rich")
  :hook (ivy-mode . ivy-filthy-rich-mode))

;; flx is used as the fuzzy-matching indexer backend for ivy.

(use-package flx
  :straight t
  :after ivy)

;; swiper is a buffer search interface using ivy.

(use-package swiper
  :straight t
  :general (:states 'normal "/" 'swiper))

;; counsel provides replacements for core Emacs commands using ivy.

(use-package counsel
  :straight t
  :commands (counsel-mode counsel-describe-face)
  :general ("M-x" #'counsel-M-x "C-x C-f" #'counsel-find-file)
  :general (:keymaps 'counsel-find-file-map
            "C-M-j" #'ivy-immediate-done
            "C-h" #'counsel-up-directory)
  :config
  (progn
    (setq counsel-yank-pop-separator (concat "\n" (make-vector 120 ?─) "\n"))
    (setf (alist-get 'counsel-yank-pop ivy-height-alist) 20)
    (counsel-mode +1)))

;; historian remembers your choices in completion menus.

(use-package historian
  :straight t
  :commands (historian-mode)
  :after ivy
  :config (historian-mode +1))

;; ivy-historian uses Historian to sort Ivy candidates by frecency+flx.

(use-package ivy-historian
  :straight t
  :commands (ivy-historian-mode)
  :after (:and ivy historian)
  :config
  (progn
    ;; Tweak historian weighting settings. These values are chosen
    ;; subjectively to produce good results.
    (setq ivy-historian-freq-boost-factor 2000)
    (setq ivy-historian-recent-boost 2000)
    (setq ivy-historian-recent-decrement 1000)

    (ivy-historian-mode 1)))

(provide 'config-ivy)

;;; config-ivy.el ends here
