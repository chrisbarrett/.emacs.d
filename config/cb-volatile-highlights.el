;;; cb-volatile-highlights.el --- Configuration for volatile highlights.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package volatile-highlights
  :straight t
  :defer 1

  :commands (volatile-highlights-mode)
  :functions (vhl/install-extension
              vhl/define-extension
              vhl/load-extension)

  :config
  (progn

    ;; Evil support.

    (vhl/define-extension 'evil
                          'evil-move
                          'evil-paste-after
                          'evil-paste-before
                          'evil-paste-pop)

    (with-eval-after-load 'evil
      (vhl/install-extension 'evil)
      (vhl/load-extension 'evil))

    ;; Undo-tree support.

    (vhl/define-extension 'undo-tree
                          'undo-tree-move
                          'undo-tree-yank)

    (with-eval-after-load 'undo-tree
      (vhl/install-extension 'undo-tree)
      (vhl/load-extension 'undo-tree))

    (volatile-highlights-mode)))

(provide 'cb-volatile-highlights)

;;; cb-volatile-highlights.el ends here
