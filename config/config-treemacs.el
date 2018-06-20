;;; config-treemacs.el --- Configuration for treemacs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'all-the-icons)
(require 'config-hydras)
(require 'dash)
(require 'general)

(use-package treemacs
  :straight t
  :commands (treemacs
             treemacs-follow-mode
             treemacs-git-mode
             treemacs-filewatch-mode)
  :preface
  (defun config-treemacs-format-icon (item)
    (-let* (((pattern f . spec) item)
            (key (s-replace-all '(("^" . "") ("\\" . "") ("$" . "") ("." . "")) pattern))
            (icon (apply f spec)))
      (cons key icon)))

  :init
  (general-setq treemacs-persist-file
                (f-join paths-cache-directory "treemacs-persist"))
  :config
  (progn
    (require 'treemacs-hacks)
    (require 'projectile)

    (general-setq
     treemacs-recenter-after-file-follow nil
     treemacs-follow-after-init t
     treemacs-silent-filewatch t
     treemacs-dotfiles-regex (rx-to-string
                              `(and bol (or (and "." (1+ any))
                                            ,@projectile-globally-ignored-directories)))
     treemacs-silent-refresh t
     treemacs-collapse-dirs (if (executable-find "python") 3 0)
     treemacs-is-never-other-window t
     treemacs-show-hidden-files nil
     treemacs-width 30)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-git-mode 'simple)

    ;; Hide ignored files.

    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)

    ;; Use all-the-icons for treemacs.

    (clrhash treemacs-icons-hash)
    (--each (-map #'config-treemacs-format-icon all-the-icons-icon-alist)
      (-let [(file-ext . icon) it]
        (treemacs-define-custom-icon icon file-ext)))

    (treemacs-define-custom-icon (all-the-icons-icon-for-mode 'web-json-mode) "json")

    (setq treemacs-icon-root-png (concat (all-the-icons-octicon "repo") "\t"))
    (setq treemacs-icon-closed-png (concat (all-the-icons-faicon "folder") "\t"))
    (setq treemacs-icon-open-png (concat (all-the-icons-faicon "folder-open") "\t"))
    (setq treemacs-icon-text (concat (all-the-icons-faicon "file-text-o") "\t"))
    (setq treemacs-icon-fallback (concat (all-the-icons-faicon "file") "\t"))

    ;; HACK: Try to line up icons.

    (defun config-treemacs-set-tab-width ()
      (setq-local tab-width 1))

    (add-hook 'treemacs-mode-hook #'config-treemacs-set-tab-width)))

(use-package treemacs-evil
  :straight t
  :after (:and treemacs evil)
  :config
  (progn
    (config-hydras-insinuate evil-treemacs-state-map)
    (setq evil-treemacs-state-cursor '("SkyBlue" box))))

(use-package treemacs-projectile
  :straight t
  :after (:and treemacs projectile))

(provide 'config-treemacs)

;;; config-treemacs.el ends here
