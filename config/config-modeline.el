;;; config-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package header-line-format
  :defines header-line-format
  :config
  (setq-default header-line-format header-line-format))

(use-package hidden-mode-line
  :commands (hidden-mode-line-mode global-hidden-mode-line-mode)
  :init
  (setq-default mode-line-format '("")))

(use-package header-line-mode
  :commands (header-line-global-mode
             header-line-mode
             header-line-mode-on)
  :demand t

  :preface
  ;; HACK: Suppress the header line in the scratch buffer.
  (defun config-modeline--hacky-hide-scratch-buffer-header-line ()
    (run-with-timer 0.001
                    nil
                    (lambda ()
                      (with-current-buffer (get-buffer-create "*scratch*")
                        (header-line-mode -1)))))

  :config
  (progn
    (header-line-global-mode +1)
    (setq header-line-function (lambda () header-line-format))
    (add-hook 'after-init-hook #'config-modeline--hacky-hide-scratch-buffer-header-line)))



(provide 'config-modeline)

;;; config-modeline.el ends here
