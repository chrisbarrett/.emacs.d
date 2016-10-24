;;; cb-neotree-cmds.el --- Commands for working with neotree.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'neotree)

(autoload 'projectile-project-root "projectile")

(defvar cb-neotree-cmds--auto-indent-point t)

(defun cb-neotree-expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when cb-neotree-cmds--auto-indent-point
              (forward-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

(defun cb-neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when cb-neotree-cmds--auto-indent-point
        (neo-point-auto-indent)))))

(defun cb-neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (cb-neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(defun cb-neotree-find-project-root ()
  "Go to the root of the current project in neotree."
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))

(provide 'cb-neotree-cmds)

;;; cb-neotree-cmds.el ends here
