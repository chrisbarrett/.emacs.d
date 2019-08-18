;;; config-restclient.el --- Configuration for restclient-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'major-mode-hydra)

(major-mode-hydra-define restclient-mode nil
  ("Execute"
   (("c" restclient-http-send-current "other window")
    ("o" restclient-http-send-current-stay-in-window "this window"))))

(use-package restclient
  :straight t
  :commands (restclient-mode
             restclient-http-send-current
             restclient-http-send-current-stay-in-window)
  :preface
  (progn
    (defconst config-restclient--js-modes
      '(js-mode js2-mode yaml-mode))

    (defun config-restclient--delete-trailing-comments ()
      (cl-labels
          ((current-line
            ()
            (buffer-substring (line-beginning-position) (line-end-position)))
           (at-commented-line-p
            ()
            (string-match-p (rx bol "//") (current-line))))

        (when (apply #'derived-mode-p config-restclient--js-modes)
          (save-excursion
            (goto-char (point-max))
            (while (or (at-commented-line-p) (string-empty-p (current-line)))
              (forward-line -1))
            (delete-region (line-beginning-position) (point-max)))))))

  :config
  (progn
    (setq restclient-same-buffer-response-name "*restclient*")

    (add-hook 'restclient-response-loaded-hook #'config-restclient--delete-trailing-comments)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*restclient*" eos)
                   (display-buffer-reuse-window
                    display-buffer-pop-up-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.66)))))

(provide 'config-restclient)

;;; config-restclient.el ends here
