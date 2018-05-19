;;; config-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package cb-header-line-format
  :defines cb-header-line-format
  :config
  (setq-default header-line-format cb-header-line-format))

(use-package hidden-mode-line
  :commands (hidden-mode-line-mode global-hidden-mode-line-mode)
  :init
  (setq-default mode-line-format " "))

(use-package cb-header-line-mode
  :commands (cb-header-line-global-mode
             cb-header-line-mode
             cb-header-line-mode-on)
  :demand t

  :preface
  ;; HACK: Suppress the header line in the scratch buffer.
  (defun config-modeline--hacky-hide-scratch-buffer-header-line ()
    (run-with-timer 0.001
                    nil
                    (lambda ()
                      (with-current-buffer (get-buffer-create "*scratch*")
                        (cb-header-line-mode -1)))))

  :config
  (progn
    (cb-header-line-global-mode +1)
    (setq cb-header-line-function (lambda () cb-header-line-format))
    (add-hook 'after-init-hook #'config-modeline--hacky-hide-scratch-buffer-header-line)))



(provide 'config-modeline)

;;; config-modeline.el ends here
