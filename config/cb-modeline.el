;;; cb-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package all-the-icons
  :straight t
  :defer t
  :init
  (defvar all-the-icons-scale-factor 1.0)
  :config
  (dolist (spec '((nix-mode all-the-icons-faicon "linux" :face all-the-icons-purple)
                  (makefile-mode all-the-icons-fileicon "gnu" :face all-the-icons-dorange)
                  (makefile-bsdmake-mode all-the-icons-fileicon "gnu" :face all-the-icons-dorange)
                  (mu4e-main-mode all-the-icons-octicon "inbox" :face all-the-icons-dsilver)
                  (mu4e-headers-mode all-the-icons-octicon "inbox" :face all-the-icons-dsilver)
                  (mu4e-view-mode all-the-icons-octicon "comment-discussion" :face all-the-icons-dsilver)
                  (mu4e-compose-mode all-the-icons-octicon "comment-discussion" :face all-the-icons-orange)))
    (add-to-list 'all-the-icons-mode-icon-alist spec)))

(use-package cb-header-line-format
  :defines cb-header-line-format
  :config
  (setq-default header-line-format cb-header-line-format))

(use-package hidden-mode-line
  :commands (hidden-mode-line-mode global-hidden-mode-line-mode)
  :init
  (setq-default mode-line-format " "))

(use-package cb-header-line-mode
  :commands (cb-header-line-global-mode cb-header-line-mode cb-header-line-mode-on)
  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "tM" #'cb-header-line-mode
      "tm" #'cb-header-line-global-mode)
    (add-hook 'after-init-hook #'cb-header-line-global-mode))
  :config
  (setq cb-header-line-function (lambda () cb-header-line-format)))

(provide 'cb-modeline)

;;; cb-modeline.el ends here
