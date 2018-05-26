;;; config-neotree.el --- Configuration for neotree.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'config-hydras)
(require 'general)

(autoload 'projectile-project-root "projectile")



;; Helper functions

(autoload 'neo-buffer--expanded-node-p "neotree")
(autoload 'neo-buffer--get-filename-current-line "neotree")
(autoload 'neo-buffer--refresh "neotree")
(autoload 'neo-buffer--set-expand "neotree")
(autoload 'neo-point-auto-indent "neotree")

(defvar config-neotree--auto-indent-point t)

(defun config-neotree-expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when config-neotree--auto-indent-point
              (forward-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

(defun config-neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when config-neotree--auto-indent-point
        (neo-point-auto-indent)))))

(defun config-neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (config-neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))



(use-package neotree
  :straight t
  :defer t
  :general
  (:keymaps 'neotree-mode-map :states 'motion
            "TAB" #'neotree-stretch-toggle
            "RET" #'neotree-enter
            "|"   #'neotree-enter-vertical-split
            "-"   #'neotree-enter-horizontal-split
            "c"   #'neotree-create-node
            "d"   #'neotree-delete-node
            "gr"  #'neotree-refresh
            "h"   #'config-neotree-collapse-or-up
            "H"   #'neotree-select-previous-sibling-node
            "j"   #'next-line
            "J"   #'neotree-select-down-node
            "k"   #'previous-line
            "K"   #'neotree-select-up-node
            "l"   #'config-neotree-expand-or-open
            "L"   #'neotree-select-next-sibling-node
            "q"   #'neotree-hide
            "r"   #'neotree-rename-node
            "R"   #'neotree-change-root
            "s"   #'neotree-hidden-file-toggle)

  :preface
  (use-package config-neotree-cmds
    :commands (config-neotree-expand-or-open
               config-neotree-collapse
               config-neotree-collapse-or-up))

  :config
  (progn
    (general-setq
     neo-window-width 30
     neo-create-file-auto-open t
     neo-banner-message "Press ? for neotree help"
     neo-show-updir-line nil
     neo-mode-line-type 'nerd
     neo-smart-open t
     neo-theme 'icons
     neo-persist-show nil
     neo-show-hidden-files nil
     neo-vc-integration nil)

    (add-to-list 'neo-hidden-regexp-list "target$")
    (add-to-list 'neo-hidden-regexp-list "__pycache__$")
    (add-to-list 'neo-hidden-regexp-list "/flycheck_$")
    (add-to-list 'neo-hidden-regexp-list "scala-2\\.[0-9]+$")

    ;; Enable leader key in neotree
    (config-hydras-insinuate neotree-mode-map)))

(provide 'config-neotree)

;;; config-neotree.el ends here
