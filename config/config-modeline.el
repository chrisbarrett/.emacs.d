;;; config-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package mode-line-format
  :defines mode-line-format
  :config
  (setq-default mode-line-format (mode-line-format)))

(use-package hidden-mode-line
  :commands (hidden-mode-line-mode global-hidden-mode-line-mode))

(use-package mode-line-mode
  :commands (mode-line-global-mode
             mode-line-mode
             mode-line-mode-off)
  :demand t
  :hook (eshell-mode . mode-line-mode-off)

  :preface
  ;; HACK: Suppress the header line in the scratch buffer.
  (defun config-modeline--hacky-hide-scratch-buffer-mode-line ()
    (run-with-timer 0.001
                    nil
                    (lambda ()
                      (with-current-buffer (get-buffer-create "*scratch*")
                        (mode-line-mode -1)))))

  :config
  (progn
    (mode-line-global-mode +1)
    (setq mode-line-function #'mode-line-format)
    (add-hook 'after-init-hook #'config-modeline--hacky-hide-scratch-buffer-mode-line)))


(provide 'config-modeline)

;;; config-modeline.el ends here
