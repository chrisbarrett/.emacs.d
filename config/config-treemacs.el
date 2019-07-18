;;; config-treemacs.el --- Configuration for treemacs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'general)

(use-package treemacs
  :straight t
  :commands (treemacs
             treemacs-follow-mode
             treemacs-git-mode
             treemacs-filewatch-mode)
  :mode-hydra
  (treemacs-mode
   ("View"
    (("." treemacs-toggle-show-dotfiles "hidden files"))
    "File"
    (("fr" treemacs-rename "rename...")
     ("fx" treemacs-delete "delete..."))
    "Projects"
    (("pa" treemacs-add-project-to-workspace "add...")
     ("pr" treemacs-rename-project "rename...")
     ("px" treemacs-remove-project-from-workspace "remove..."))))

  :preface
  (defun config-treemacs-format-icon (item)
    (-let* (((pattern f . spec) item)
            (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) pattern))
            (icon (apply f spec)))
      (cons key icon)))
  :preface
  (defun config-treemacs--setup-buffer ()
    (setq-local tab-width 1)
    (setq-local cursor-type nil))

  :hook
  (treemacs-mode . config-treemacs--setup-buffer)

  :init
  (let ((cache-dir (f-join paths-cache-directory "treemacs")))
    (f-mkdir cache-dir)
    (general-setq treemacs-persist-file (f-join cache-dir "treemacs-persist")
                  treemacs--last-error-persist-file (f-join cache-dir "treemacs-errors")))

  :config
  (progn
    (general-setq treemacs-dotfiles-regex (rx-to-string
                                           `(and bol (or (and "." (1+ any))
                                                         ,@projectile-globally-ignored-directories))))

    (general-setq
     treemacs-python-executable (executable-find "python3")

     ;; Disable the indicator next to open files--hl-line is sufficient.
     treemacs-fringe-indicator-mode nil

     treemacs-recenter-after-file-follow nil
     treemacs-follow-after-init t
     treemacs-silent-filewatch t
     treemacs-silent-refresh t
     treemacs-collapse-dirs (if (executable-find "python") 3 0)
     treemacs-is-never-other-window t
     treemacs-show-hidden-files nil
     treemacs-width 30)

    (treemacs-fringe-indicator-mode t)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'simple)

    ;; Hide ignored files.

    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)

    ;; HACK: Use all-the-icons for treemacs. Add tabs to the end of each icon so
    ;; that text is more likely to be aligned.

    (when (boundp 'treemacs-icons-hash)
      (clrhash treemacs-icons-hash))
    (--each (-map #'config-treemacs-format-icon all-the-icons-icon-alist)
      (-let [(file-ext . icon) it]
        (treemacs-define-custom-icon icon file-ext)))

    (setq treemacs-icon-root-png (concat (all-the-icons-octicon "repo") "\t"))
    (setq treemacs-icon-closed-png (concat (all-the-icons-faicon "folder") "\t"))
    (setq treemacs-icon-open-png (concat (all-the-icons-faicon "folder-open") "\t"))
    (setq treemacs-icon-text (concat (all-the-icons-faicon "file-text-o") "\t"))
    (setq treemacs-icon-fallback (concat (all-the-icons-faicon "file") "\t"))))

(use-package treemacs-evil
  :straight t
  :after (:and treemacs evil)
  :defer t
  :preface
  (defun config-treemacs--buffer-setup ()
    (require 'treemacs-evil)
    (setq evil-treemacs-state-cursor (list (face-background 'hl-line) nil))
    (evil-treemacs-state +1))
  :hook
  (treemacs-mode . config-treemacs--buffer-setup)
  :config
  (progn
    (evil-define-key 'treemacs treemacs-mode-map (kbd "J") 'treemacs-next-project)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "K") 'treemacs-previous-project)))

(use-package treemacs-projectile
  :defer t
  :after (:and treemacs projectile))

(provide 'config-treemacs)

;;; config-treemacs.el ends here
