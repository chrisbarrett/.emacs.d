;;; cb-media.el --- Configuration for media & external integrations  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :general
  (:states '(normal) :keymaps 'pdf-view-mode-map
   "+" 'pdf-view-enlarge
   "-" 'pdf-view-shrink
   "0" 'pdf-view-scale-reset
   "j" 'pdf-view-next-line-or-next-page
   "k" 'pdf-view-previous-line-or-previous-page
   "n" 'pdf-view-next-page-command
   "p" 'pdf-view-previous-page-command
   "gr" 'revert-buffer
   "gg" 'pdf-view-first-page
   "gG" 'pdf-view-last-page
   "J" 'pdf-view-goto-page
   "q" 'kill-this-buffer)
  :config
  (evil-set-initial-state 'pdf-view-mode 'normal))

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

(provide 'cb-media)

;;; cb-media.el ends here
