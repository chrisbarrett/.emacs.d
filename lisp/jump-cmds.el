;;; jump-cmds.el --- Shortcut commands to go to particular locations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defun jump-to-init-file ()
  "Open the Emacs init.el file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun jump-to-nix-packages ()
  "Open the nix packages file."
  (interactive)
  (find-file "~/Sync/nix/packages.nix"))

(defun jump-to-personal-config ()
  "Open the personal configuration file."
  (interactive)
  (find-file "~/Sync/emacs/personal-config.el"))

(defun jump-to-messages ()
  "Open the messages buffer."
  (interactive)
  (display-buffer "*Messages*"))

(provide 'jump-cmds)

;;; jump-cmds.el ends here
