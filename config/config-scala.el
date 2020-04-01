;;; config-scala.el --- Config for Scala language  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package scala-mode
  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :preface
  (defun config-scala--set-up-mode ()
    (add-hook 'before-save-hook #'lsp-format-buffer nil t))

  :config
  (progn
    (add-hook 'scala-mode-hook #'config-scala--set-up-mode)

    ;; Disable new shell features which break tab completion.
    ;; See: https://github.com/hvesalai/emacs-sbt-mode/issues/152
    (setq sbt:program-options '("-Dsbt.supershell=false"))

    ;; HACK: allows using SPACE when in the minibuffer
    (substitute-key-definition 'minibuffer-complete-word 'self-insert-command minibuffer-local-completion-map)))

(provide 'config-scala)

;;; config-scala.el ends here
