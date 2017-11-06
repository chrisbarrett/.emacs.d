;;; cb-mu4e.el --- Configuration for mu4e.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-emacs)
(require 'spacemacs-keys)
(require 'evilified-state)
(require 'f)

(add-to-list 'load-path (concat cb-emacs-site-lisp-directory "/mu4e") t)

(use-package mu4e
  :commands (mu4e mu4e-compose-new)
  :defer t

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

    (defun cb-mu4e-shr-buffer ()
      (let ((shr-use-fonts nil))
        (shr-render-region (point-min) (point-max))))

    (defun cb-mu4e--insert-signature-before-quoted-message ()
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

    (defun cb-mu4e-view-ret ()
      "Call the command that would be run by a mouse click at point."
      (interactive)
      (-if-let ((&alist 'keymap (&alist 'mouse-1 action)) (text-properties-at (point)))
          (call-interactively action)
        (call-interactively #'evil-ret)))

    (use-package cb-mu4e-utils
      :after mu4e
      :functions (cb-mu4e-utils-view-in-external-browser-action
                  cb-mu4e-utils-read-and-archive-action
                  mu4e-view-open-attachment
                  mu4e-headers-mark-for-read-and-archive
                  mu4e-view-mark-for-read-and-archive)))

  :init
  (spacemacs-keys-set-leader-keys "am" #'mu4e)

  :config
  (progn
    (evilified-state-evilify-map mu4e-main-mode-map
      :mode mu4e-main-mode
      :bindings
      (kbd "j") #'mu4e~headers-jump-to-maildir
      (kbd "q") #'bury-buffer)

    (evilified-state-evilify-map mu4e-headers-mode-map
      :mode mu4e-headers-mode
      :bindings
      (kbd "J") #'mu4e~headers-jump-to-maildir
      (kbd "j") #'mu4e-headers-next
      (kbd "k") #'mu4e-headers-prev)

    (evil-set-initial-state 'mu4e-view-mode 'motion)
    (evil-define-key 'motion mu4e-view-mode-map
      (kbd "J") #'mu4e~view-headers-jump-to-maildir
      (kbd "n") #'mu4e-view-headers-next
      (kbd "p") #'mu4e-view-headers-prev
      (kbd "C-j") #'mu4e-view-headers-next
      (kbd "C-k") #'mu4e-view-headers-prev
      ;; (kbd "w") #'evil-forward-word-begin
      ;; (kbd "b") #'evil-backward-word-begin
      (kbd "RET") #'cb-mu4e-view-ret)

    ;; Set variables

    (setq mu4e-use-fancy-chars t)
    (setq mu4e-headers-attach-mark (purecopy '("a" . "A")))
    (setq mu4e-headers-unread-mark (purecopy '("u" . "●")))
    (setq mu4e-headers-seen-mark (purecopy '(" " . " ")))
    (setq mu4e-hide-index-messages t)
    (setq mu4e-headers-skip-duplicates t)
    (setq mu4e-compose-format-flowed t)
    (setq mu4e-completing-read-function 'completing-read)

    (setq mu4e-view-prefer-html t)
    (setq mu4e-view-show-images t)
    (setq mu4e-view-show-addresses t)
    (setq message-kill-buffer-on-exit t)

    (setq mu4e-maildir (f-expand "~/Maildir"))
    (setq mu4e-headers-date-format "%d-%m-%y %k:%M")
    (setq sendmail-program "msmtp")
    (setq message-send-mail-function #'message-send-mail-with-sendmail)

    (setq mu4e-bookmarks
          '(("flag:unread AND ((s:JIRA AND b:chrisb) OR (NOT (s:JIRA OR s:jenkins))) AND (NOT (m:/walrus/trash OR m:/movio/trash))"
             "Unread messages" ?u)
            ("d:today..now AND NOT (s:JIRA OR s:jenkins)"
             "Today's messages" ?t)
            ("d:7d..now AND NOT (s:JIRA OR s:jenkins)"
             "Last 7 days" ?w)
            ("d:30d..now AND NOT (s:JIRA OR s:jenkins)"
             "Last 30 days" ?m)
            ("m:/walrus/inbox"
             "Inbox" ?i)
            ("m:/walrus/inbox OR m:/movio/inbox"
             "All Inboxes" ?I)
            ("m:/walrus/sent OR m:/movio/sent"
             "Sent messages" ?s)
            ("bitbucket OR github"
             "Code & PRs" ?c)
            ("d:7d..now AND s:JIRA AND b:chrisb AND m:/movio/jira"
             "JIRA - mentions" ?j)
            ("d:7d..now AND m:/movio/jira"
             "JIRA - all" ?J)))

    ;; All my mailservers use IMAP. Use mbsync to synchronise mail between the
    ;; server and my local machine.
    (setq mu4e-get-mail-command "mbsync -V -q -a")
    (setq mu4e-change-filenames-when-moving t)

    (setq smtpmail-queue-mail nil)
    (setq smtpmail-queue-dir (concat mu4e-maildir "/queue/cur"))

    ;; Save attachments to Downloads dir.
    (setq mu4e-attachment-dir (f-expand "~/Downloads"))

    ;; Put quoted messages after signature.
    (setq message-forward-before-signature nil)

    ;; Use standard citation style.
    (setq message-citation-line-function #'message-insert-formatted-citation-line)
    (setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")

    ;; Update every 2 minutes.
    (setq mu4e-update-interval (* 60 5))

    ;; Use word wrap instead of auto-fill.
    (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'mu4e-compose-mode-hook (lambda () (setq word-wrap t)))

    ;; Wrap lines when viewing.
    (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

    ;; Put signature before quoted messages.
    (add-hook 'mu4e-compose-mode-hook #'cb-mu4e--insert-signature-before-quoted-message)

    ;; use imagemagick, if available
    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    ;; Ensure I'm never prompted for the buffer coding system when sending mail.
    (setq sendmail-coding-system 'utf-8)

    ;; Custom rendering of HTML messages
    (setq mu4e-html2text-command #'cb-mu4e-shr-buffer)

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

    ;; Declare archive functions.

    (define-key mu4e-headers-mode-map (kbd "r") #'mu4e-headers-mark-for-read-and-archive)
    (define-key mu4e-view-mode-map (kbd "r") #'mu4e-view-mark-for-read-and-archive)

    ;; Enable leader key in mu4e maps
    (define-key mu4e-headers-mode-map (kbd "SPC") spacemacs-keys-default-map)
    (define-key mu4e-view-mode-map (kbd "SPC") spacemacs-keys-default-map)
    (define-key mu4e-main-mode-map (kbd "SPC") spacemacs-keys-default-map)
    (define-key mu4e-headers-mode-map (kbd "SPC") spacemacs-keys-default-map)

    ;; Define some leader keybindings.

    (spacemacs-keys-set-leader-keys-for-major-mode 'mu4e-view-mode
      "a" #'mu4e-view-open-attachment
      "o" #'cb-mu4e-utils-view-in-external-browser-action)

    (spacemacs-keys-set-leader-keys-for-major-mode 'mu4e-compose-mode
      "a" 'mail-add-attachment
      "i" 'mail-insert-file)))

(use-package org-mu4e
  :after org)

(provide 'cb-mu4e)

;;; cb-mu4e.el ends here
