;;; cb-restclient.el --- Configuration for restclient-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'spacemacs-keys)

(use-package restclient
  :straight t
  :commands (restclient-mode
             restclient-http-send-current
             restclient-http-send-current-stay-in-window)
  :preface
  (progn
    (defconst cb-restclient--js-modes
      '(js-mode js2-mode cb-web-js-base-mode cb-web-json-mode yaml-mode))

    (defun cb-restclient--delete-trailing-comments ()
      (cl-labels
          ((current-line
            ()
            (buffer-substring (line-beginning-position) (line-end-position)))
           (at-commented-line-p
            ()
            (string-match-p (rx bol "//") (current-line))))

        (when (apply #'derived-mode-p cb-restclient--js-modes)
          (save-excursion
            (goto-char (point-max))
            (while (or (at-commented-line-p) (string-empty-p (current-line)))
              (forward-line -1))
            (delete-region (line-beginning-position) (point-max)))))))

  :config
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'restclient-mode
      "c" #'restclient-http-send-current
      "o" #'restclient-http-send-current-stay-in-window)

    (setq restclient-same-buffer-response-name "*restclient*")

    (add-hook 'restclient-response-loaded-hook #'cb-restclient--delete-trailing-comments)

    (with-eval-after-load 'which-key
      (with-no-warnings
        (push `((nil . ,(rx bos "restclient-http-" (group (+ nonl)))) . (nil . "\\1"))
              which-key-replacement-alist)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*restclient*" eos)
                   (display-buffer-reuse-window
                    display-buffer-pop-up-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.66)))))

(provide 'cb-restclient)

;;; cb-restclient.el ends here
