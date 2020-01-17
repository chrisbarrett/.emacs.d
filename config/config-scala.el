;;; config-scala.el --- Config for Scala language  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package scala-mode
  :straight t
  :mode ("\\.\\(scala\\|sbt\\)\\'" . scala-mode)
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :straight t
  :commands (sbt-start sbt-command)
  :config
  (progn
    ;; Disable new shell features which break tab completion.
    ;; See: https://github.com/hvesalai/emacs-sbt-mode/issues/152
    (setq sbt:program-options '("-Dsbt.supershell=false"))

    ;; HACK: allows using SPACE when in the minibuffer
    (substitute-key-definition 'minibuffer-complete-word 'self-insert-command minibuffer-local-completion-map)))

(provide 'config-scala)

;;; config-scala.el ends here
