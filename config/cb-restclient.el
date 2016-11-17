;;; cb-restclient.el --- Configuration for restclient-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package restclient
  :commands (restclient-mode
             restclient-http-send-current
             restclient-http-send-current-stay-in-window)
  :config
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'restclient-mode
      "c" #'restclient-http-send-current
      "o" #'restclient-http-send-current-stay-in-window)

    (setq restclient-same-buffer-response-name "*restclient*")

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*restclient*" eos)
                   (display-buffer-reuse-window
                    display-buffer-pop-up-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.66)))))


(provide 'cb-restclient)

;;; cb-restclient.el ends here
