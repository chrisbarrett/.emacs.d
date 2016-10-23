;;; cb-neotree.el --- Configuration for neotree.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'evil-transient-state)
(require 'evilified-state)
(require 'spacemacs-keys)

(use-package neotree
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

  :functions (neo-global--window-exists-p)

  :preface
  (progn
    (use-package cb-neotree-cmds
      :commands (cb-neotree-neotree-expand-or-open
                 cb-neotree-neotree-collapse
                 cb-neotree-neotree-collapse-or-up
                 cb-neotree-neotree-find-project-root)))

  :config
  (progn
    (setq neo-window-width 32)
    (setq neo-create-file-auto-open t)
    (setq neo-banner-message "Press ? for neotree help")
    (setq neo-show-updir-line nil)
    (setq neo-mode-line-type 'neotree)
    (setq neo-smart-open t)
    (setq neo-dont-be-alone t)
    (setq neo-persist-show nil)
    (setq neo-show-hidden-files nil)
    (setq neo-modern-sidebar t)
    (setq neo-vc-integration nil)


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
      (kbd "h")   #'cb-neotree-neotree-collapse-or-up
      (kbd "H")   #'neotree-select-previous-sibling-node
      (kbd "j")   #'next-line
      (kbd "J")   #'neotree-select-down-node
      (kbd "k")   #'previous-line
      (kbd "K")   #'neotree-select-up-node
      (kbd "l")   #'cb-neotree-neotree-expand-or-open
      (kbd "L")   #'neotree-select-next-sibling-node
      (kbd "q")   #'neotree-hide
      (kbd "r")   #'neotree-rename-node
      (kbd "R")   #'neotree-change-root
      (kbd "?")   #'cb-neotree-neotree-transient-state/body
      (kbd "s")   #'neotree-hidden-file-toggle))

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "ft" #'neotree-toggle
      "pt" #'cb-neotree-neotree-find-project-root)

    (evil-transient-state-define neotree
      :title "NeoTree Key Hints"
      :doc "
Navigation^^^^             Actions^^         Visual actions/config^^^
───────^^^^─────────────── ───────^^──────── ───────^^^────────────────
[_L_]   next sibling^^     [_c_] create      [_TAB_] shrink/enlarge
[_H_]   previous sibling^^ [_d_] delete      [_|_]   vertical split
[_J_]   goto child^^       [_r_] rename      [_-_]   horizonatal split
[_K_]   goto parent^^      [_R_] change root [_gr_]  refresh^
[_l_]   open/expand^^      ^^                [_s_]   hidden:^^^ %s(if neo-buffer--show-hidden-file-p \"on\" \"off\")
[_h_]   up/collapse^^      ^^                ^^^
[_j_]   line down^^        ^^                ^^^
[_k_]   line up^^          ^^                ^^
[_RET_] open               ^^^^              [_?_]   close hints
"
        :bindings
        ("RET" neotree-enter)
        ("TAB" neotree-stretch-toggle)
        ("|" neotree-enter-vertical-split)
        ("-" neotree-enter-horizontal-split)
        ("?" nil :exit t)
        ("c" neotree-create-node)
        ("d" neotree-delete-node)
        ("gr" neotree-refresh)
        ("h" cb-neotree-neotree-collapse-or-up)
        ("H" neotree-select-previous-sibling-node)
        ("j" next-line)
        ("J" neotree-select-down-node)
        ("k" previous-line)
        ("K" neotree-select-up-node)
        ("l" cb-neotree-neotree-expand-or-open)
        ("L" neotree-select-next-sibling-node)
        ("r" neotree-rename-node)
        ("R" neotree-change-root)
        ("s" neotree-hidden-file-toggle))))

(provide 'cb-neotree)

;;; cb-neotree.el ends here
