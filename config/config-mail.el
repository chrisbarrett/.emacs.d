;;; config-mail.el --- Configuration for mail client software.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'paths)
(require 'major-mode-hydra)
(require 'display-buffer-fullframe)

(cl-eval-when (compile)
  (require 'mu4e)
  (require 'mu4e-view)
  (require 'mu4e-headers))

(autoload 'mail-add-attachment "sendmail")
(autoload 'mu4e-view-open-attachment "mu4e-view")

(defun config-mail--view-in-external-browser-action (msg)
  "View the current message MSG in the browser."
  (interactive (list mu4e~view-msg))
  (let ((browse-url-browser-function #'browse-url-default-browser))
    (mu4e-action-view-in-browser msg)))

(major-mode-hydra-define mu4e-view-mode nil
  ("View"
   (("a" mu4e-view-open-attachment "open attachment")
    ("o" config-mail--view-in-external-browser-action "open in browser"))))

(major-mode-hydra-define mu4e-compose-mode nil
  ("Attachments"
   (("a" mail-add-attachment "add"))))

(defconst config-mail-unread-mail-query
  "flag:unread AND NOT (flag:trashed OR m:/walrus/Archive)")

;; `mu4e' is an Emacs mail client. I install the lisp along with the 'mu'
;; program via Nix.

(add-to-list 'load-path (f-join paths-site-lisp-directory "mu4e"))

(general-setq mu4e-bookmarks `((,config-mail-unread-mail-query
                                "Unread messages" ?u)
                               ("d:today..now"
                                "Today's messages" ?t)
                               ("d:7d..now AND NOT s:JIRA"
                                "Last 7 days" ?w)
                               ("d:30d..now AND NOT s:JIRA"
                                "Last 30 days" ?m)
                               ("m:/walrus/Inbox"
                                "Inbox" ?i)
                               ("m:/walrus/Notifications"
                                "Notifications" ?n)
                               ("m:/walrus/Sent"
                                "Sent messages" ?s)
                               ("bitbucket OR github"
                                "Code & PRs" ?c)))

(use-package mu4e
  :commands (mu4e mu4e-compose-new)
  :preface
  (progn
    (defun config-mail--shr-buffer ()
      (let ((shr-use-fonts nil))
        (shr-render-region (point-min) (point-max))))

    (defun config-mail--insert-signature-before-quoted-message ()
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
              (insert "\n")))))))
  :init
  (global-set-key [remap compose-mail] #'mu4e-compose-new)

  :config
  (progn
    (add-hook 'mu4e-compose-mode-hook #'config-mail--insert-signature-before-quoted-message)

    ;; Use word wrap instead of auto-fill.
    (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'mu4e-compose-mode-hook (lambda () (setq word-wrap t)))

    ;; Wrap lines when viewing.
    (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

    (general-setq mu4e-context-policy 'pick-first
                  mu4e-compose-context-policy 'ask-if-none
                  mu4e-compose-format-flowed t
                  message-kill-buffer-on-exit t
                  mu4e-use-fancy-chars t
                  mu4e-headers-include-related nil
                  mu4e-headers-attach-mark '("a" . "A")
                  mu4e-headers-unread-mark '("u" . "●")
                  mu4e-headers-seen-mark '(" " . " ")
                  mu4e-hide-index-messages t
                  mu4e-headers-skip-duplicates t
                  mu4e-index-lazy-check t
                  mu4e-confirm-quit t
                  mu4e-view-prefer-html t
                  mu4e-view-show-images t
                  mu4e-view-show-addresses t
                  mu4e-maildir (f-expand "~/Maildir")
                  mu4e-headers-date-format "%d-%m-%y %k:%M"
                  mu4e-completing-read-function #'completing-read
                  sendmail-program "msmtp"
                  message-send-mail-function #'message-send-mail-with-sendmail

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
                  mu4e-html2text-command #'config-mail--shr-buffer)

    (global-set-key [remap mu4e-quit] #'bury-buffer)

    ;; View html message in eww. `av` in view to activate
    (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

    ;; View html message in external browser. `a&` in view to activate
    (add-to-list 'mu4e-view-actions '("&viewInExternalBrowser" . config-mail--view-in-external-browser-action) t)

    ;; Make refiling mark messages as read.
    (setf (alist-get 'refile mu4e-marks)
          '(:char ("r" . "▶")
            :prompt "refile"
            :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
            :action (lambda (docid msg target)
                      (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos " *mu4e-main*" eos)
                   (display-buffer-reuse-window
                    display-buffer-fullframe)
                   (reusable-frames . visible)))))

;; `org-mu4e' provides org link integration.

(use-package org-mu4e
  :after (:any org mu4e))

;; `mu4e-alert' implements a notification system for new emails

(use-package mu4e-alert
  :straight t
  :after mu4e
  :custom
  ((mu4e-alert-interesting-mail-query config-mail-unread-mail-query))
  :config (mu4e-alert-enable-mode-line-display))

;; `messages-are-flowing' displays newline symbols in the buffer for hard newlines.

(use-package messages-are-flowing
  :straight t
  :hook (message-mode . messages-are-flowing-use-and-mark-hard-newlines))

(provide 'config-mail)

;;; config-mail.el ends here
