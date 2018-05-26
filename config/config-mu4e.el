;;; config-mu4e.el --- Configuration for mu4e.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-major-mode-hydra)
(require 'config-hydras)
(require 'f)
(require 'general)
(require 'paths)

(autoload 'mail-add-attachment "sendmail")



(cb-major-mode-hydra-define mu4e-view-mode
  "View"
  (("a" mu4e-view-open-attachment "open attachment")
   ("o" cb-mu4e-utils-view-in-external-browser-action "open in browser")))

(cb-major-mode-hydra-define mu4e-compose-mode
  "Attachments"
  (("a" mail-add-attachment "add")))



(push (f-join paths-site-lisp-directory "mu4e") load-path)

(use-package mu4e
  :straight t
  :commands (mu4e mu4e-compose-new)

  :general
  (:keymaps 'mu4e-main-mode-map :states 'emacs
            "j" #'mu4e~headers-jump-to-maildir
            "q" #'bury-buffer)
  (:keymaps 'mu4e-headers-mode-map :states 'emacs
            "J" #'mu4e~headers-jump-to-maildir
            "j" #'mu4e-headers-next
            "k" #'mu4e-headers-prev)
  (:keymaps 'mu4e-view-mode-map :states 'motion
            "F" #'mu4e-compose-forward
            "J" #'mu4e~view-headers-jump-to-maildir
            "j" #'mu4e-view-headers-next
            "k" #'mu4e-view-headers-prev
            "C-j" #'mu4e-view-headers-next
            "C-k" #'mu4e-view-headers-prev
            "RET" #'config-mu4e-view-ret)
  ;; Evil delete command is super slow for some reason. :(
  (:keymaps 'mu4e-compose-mode-map :states 'insert "<backspace>" #'backward-delete-char)
  ;; Declare archive functions.
  (:keymaps 'mu4e-headers-mode-map "r" #'mu4e-headers-mark-for-read-and-archive)
  (:keymaps 'mu4e-view-mode-map "r" #'mu4e-view-mark-for-read-and-archive)

  :preface
  (progn
    (autoload 'message-goto-body "message")
    (autoload 'message-goto-signature "message")
    (autoload 'message-insert-formatted-citation-line "message")
    (autoload 'message-insert-signature "message")
    (autoload 'message-send-mail-with-sendmail "message")
    (autoload 'shr-render-region "shr")

    ;; Declare dynamic variables

    (defvar message-signature)
    (defvar mu4e-compose-signature)
    (defvar mu4e-compose-signature-auto-include)
    (defvar shr-use-fonts)

    (defun config-mu4e-shr-buffer ()
      (let ((shr-use-fonts nil))
        (shr-render-region (point-min) (point-max))))

    (defun config-mu4e--insert-signature-before-quoted-message ()
      (unless (member mu4e-compose-type '(edit resend))
        (save-excursion
          (save-restriction
            (widen)
            (cond
             ((eq mu4e-compose-type 'new)
              (message-goto-body)
              (kill-region (point) (point-max)))
             ((message-goto-signature)
              (forward-line -2)
              (delete-region (point) (point-max))))

            (message-goto-body)
            (insert "\n")
            (narrow-to-region (point-min) (point))

            (let ((message-signature t)
                  (mu4e-compose-signature t)
                  (mu4e-compose-signature-auto-include t))
              (message-insert-signature))

            (when (member mu4e-compose-type '(forward reply))
              (goto-char (point-max))
              (insert "\n"))))))

    (defun config-mu4e-view-ret ()
      "Call the command that would be run by a mouse click at point."
      (interactive)
      (-if-let ((&alist 'keymap (&alist 'mouse-1 action)) (text-properties-at (point)))
          (call-interactively action)
        (call-interactively #'evil-ret))))
  :config
  (progn
    (general-setq
     mu4e-use-fancy-chars t
     mu4e-headers-attach-mark (purecopy '("a" . "A"))
     mu4e-headers-unread-mark (purecopy '("u" . "●"))
     mu4e-headers-seen-mark (purecopy '(" " . " "))
     mu4e-hide-index-messages t
     mu4e-headers-skip-duplicates t
     mu4e-compose-format-flowed t
     mu4e-completing-read-function 'completing-read
     mu4e-index-lazy-check t

     mu4e-view-prefer-html t
     mu4e-view-show-images t
     mu4e-view-show-addresses t
     message-kill-buffer-on-exit t

     mu4e-maildir (f-expand "~/Maildir")
     mu4e-headers-date-format "%d-%m-%y %k:%M"
     sendmail-program "msmtp"
     message-send-mail-function #'message-send-mail-with-sendmail

     mu4e-bookmarks
     '(("flag:unread AND ((s:JIRA AND b:chrisb) OR (NOT (s:JIRA OR s:jenkins))) AND (NOT m:/walrus/trash)"
        "Unread messages" ?u)
       ("d:today..now AND NOT (s:JIRA OR s:jenkins)"
        "Today's messages" ?t)
       ("d:7d..now AND NOT (s:JIRA OR s:jenkins)"
        "Last 7 days" ?w)
       ("d:30d..now AND NOT (s:JIRA OR s:jenkins)"
        "Last 30 days" ?m)
       ("m:/walrus/inbox"
        "Inbox" ?i)
       ("m:/walrus/sent"
        "Sent messages" ?s)
       ("bitbucket OR github"
        "Code & PRs" ?c))

     ;; All my mailservers use IMAP. Use mbsync to synchronise mail between the
     ;; server and my local machine.
     mu4e-get-mail-command "mbsync -V -q -a"
     mu4e-change-filenames-when-moving t

     smtpmail-queue-mail nil
     smtpmail-queue-dir (concat mu4e-maildir "/queue/cur")

     ;; Save attachments to Downloads dir.
     mu4e-attachment-dir (f-expand "~/Downloads")

     ;; Put quoted messages after signature.
     message-forward-before-signature nil

     ;; Use standard citation style.
     message-citation-line-function #'message-insert-formatted-citation-line
     message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"

     ;; Update every 2 minutes.
     mu4e-update-interval (* 60 5)

     ;; Ensure I'm never prompted for the buffer coding system when sending mail.
     sendmail-coding-system 'utf-8

     ;; Custom rendering of HTML messages
     mu4e-html2text-command #'config-mu4e-shr-buffer)

    ;; Use word wrap instead of auto-fill.
    (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'mu4e-compose-mode-hook (lambda () (setq word-wrap t)))

    ;; Wrap lines when viewing.
    (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

    ;; Use imagemagick, if available
    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    ;; Put signature before quoted messages.
    (add-hook 'mu4e-compose-mode-hook #'config-mu4e--insert-signature-before-quoted-message)

    ;; View html message in eww. `av` in view to activate
    (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

    ;; View html message in external browser. `a&` in view to activate

    (add-to-list 'mu4e-view-actions '("&viewInExternalBrowser" . cb-mu4e-utils-view-in-external-browser-action) t)

    ;; Add read+archive mark
    (add-to-list 'mu4e-marks
                 '(read-and-archive
                   :char       "r"
                   :prompt     "rArchive"
                   :show-target (lambda (target) "archive")
                   :action      cb-mu4e-utils-read-and-archive-action))

    ;; Enable leader key in mu4e maps
    (config-hydras-insinuate mu4e-headers-mode-map)
    (config-hydras-insinuate mu4e-view-mode-map)
    (config-hydras-insinuate mu4e-main-mode-map)))

(use-package cb-mu4e-utils
  :after mu4e
  :functions (cb-mu4e-utils-view-in-external-browser-action
              cb-mu4e-utils-read-and-archive-action
              mu4e-view-open-attachment
              mu4e-headers-mark-for-read-and-archive
              mu4e-view-mark-for-read-and-archive))

(use-package org-mu4e
  :after (:and org mu4e))

(provide 'config-mu4e)

;;; config-mu4e.el ends here
