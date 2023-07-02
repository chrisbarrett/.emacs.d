;;; cb-media.el --- Configuration for media & external integrations  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)

(use-package pdf-tools
  :general
  (:states '(normal) :keymaps 'pdf-view-mode-map
   "n" 'pdf-view-next-page-command
   "p" 'pdf-view-previous-page-command
   "gr" 'revert-buffer)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-scaling t)
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  (pdf-tools-install))

(use-package image
  :general (:keymaps 'image-mode-map :states '(normal motion)
            "-" #'image-decrease-size
            "+" #'image-increase-size)
  :custom
  (image-use-external-converter t))

(use-package time
  :custom
  (world-clock-list '(("Pacific/Auckland" "NZT")
                      ("Australia/Sydney" "Sydney")
                      ("America/Los_Angeles" "Pacific Time")
                      ("UTC" "UTC"))))

(use-package browse-url
  ;; Use default system mail on Darwin
  :if (equal system-type 'darwin)
  :custom
  (browse-url-mailto-function (lambda (link &rest _)
                                (start-process "open" nil "open" link))))

(use-package shell-maker
  :custom
  (shell-maker-history-path (no-littering-expand-var-file-name "shell-maker")))

(use-package chatgpt-shell
  :commands (chatgpt-shell)
  :custom
  (chatgpt-shell-display-function #'display-buffer)
  (chatgpt-shell-chatgpt-streaming t)
  (chatgpt-shell-openai-key
   (lambda ()
     (auth-source-pick-first-password :host "api.openai.com"))))

(use-package ob-chatgpt-shell
  :autoload (ob-chatgpt-shell-setup org-babel-execute:chatgpt-shell)
  :config
  (ob-chatgpt-shell-setup))

(provide 'cb-media)

;;; cb-media.el ends here
