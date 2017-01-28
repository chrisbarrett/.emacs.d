;;; cb-goto.el --- Shortcut commands to go to particular locations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defun cb-goto-init-file ()
  "Open the Emacs init.el file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun cb-goto-personal-config ()
  "Open the personal configuration file."
  (interactive)
  (find-file "~/Sync/emacs/personal-config.el"))

(defun cb-goto-messages ()
  "Open the messages buffer."
  (interactive)
  (display-buffer "*Messages*"))

(provide 'cb-goto)

;;; cb-goto.el ends here
