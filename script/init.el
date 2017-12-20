;;; init.el --- minimal company-tern configuration

;;; Commentary:

;;; Code:

(require 'cask)

(let ((source-directory (locate-dominating-file load-file-name "Cask")))
  (cask-initialize source-directory)
  (add-to-list 'load-path source-directory))

;; Tern settings.

(add-hook 'js-mode-hook 'tern-mode)

;; Company settings.

(global-company-mode)

(setq company-tooltip-align-annotations t)

(add-to-list 'company-backends 'company-tern)

;;; init.el ends here
