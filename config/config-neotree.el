;;; config-neotree.el --- Configuration for neotree.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'config-hydras)
(require 'evilified-state)

(use-package neotree
  :straight t
  :defer t
  :commands (neotree-change-root
             neotree-create-node
             neotree-delete-node
             neotree-enter
             neotree-enter-horizontal-split
             neotree-enter-vertical-split
             neotree-hidden-file-toggle
             neotree-hide
             neotree-refresh
             neotree-rename-node
             neotree-select-down-node
             neotree-select-next-sibling-node
             neotree-select-previous-sibling-node
             neotree-select-up-node
             neotree-stretch-toggle
             neotree-toggle)

  :preface
  (progn
    (use-package cb-neotree-cmds
      :commands (cb-neotree-expand-or-open
                 cb-neotree-collapse
                 cb-neotree-collapse-or-up
                 cb-neotree-find-project-root)))

  :config
  (progn
    (setq neo-window-width 30)
    (setq neo-create-file-auto-open t)
    (setq neo-banner-message "Press ? for neotree help")
    (setq neo-show-updir-line nil)
    (setq neo-mode-line-type 'nerd)
    (setq neo-smart-open t)
    (setq neo-theme 'icons)
    (setq neo-persist-show nil)
    (setq neo-show-hidden-files nil)
    (setq neo-vc-integration nil)

    (add-to-list 'neo-hidden-regexp-list "target$")
    (add-to-list 'neo-hidden-regexp-list "__pycache__$")
    (add-to-list 'neo-hidden-regexp-list "/flycheck_$")
    (add-to-list 'neo-hidden-regexp-list "scala-2\\.[0-9]+$")

    ;; Enable leader key in neotree
    (config-hydras-insinuate neotree-mode-map)

    (evilified-state-evilify-map neotree-mode-map
      :mode neotree-mode
      :bindings
      (kbd "TAB") #'neotree-stretch-toggle
      (kbd "RET") #'neotree-enter
      (kbd "|")   #'neotree-enter-vertical-split
      (kbd "-")   #'neotree-enter-horizontal-split
      (kbd "c")   #'neotree-create-node
      (kbd "d")   #'neotree-delete-node
      (kbd "gr")  #'neotree-refresh
      (kbd "h")   #'cb-neotree-collapse-or-up
      (kbd "H")   #'neotree-select-previous-sibling-node
      (kbd "j")   #'next-line
      (kbd "J")   #'neotree-select-down-node
      (kbd "k")   #'previous-line
      (kbd "K")   #'neotree-select-up-node
      (kbd "l")   #'cb-neotree-expand-or-open
      (kbd "L")   #'neotree-select-next-sibling-node
      (kbd "q")   #'neotree-hide
      (kbd "r")   #'neotree-rename-node
      (kbd "R")   #'neotree-change-root
      (kbd "?")   #'cb-neotree-transient-state/body
      (kbd "s")   #'neotree-hidden-file-toggle)))

(provide 'config-neotree)

;;; config-neotree.el ends here
