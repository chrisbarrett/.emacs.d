;;; cb-mu4e.el --- Configuration for mu4e.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-emacs)

  (defconst cb-mu4e-load-path (concat cb-emacs-lisp-directory "/mu/mu4e")))

(require 'spacemacs-keys)
(require 'evilified-state)
(require 'f)

(use-package mu4e
  :load-path cb-mu4e-load-path
  :commands (mu4e mu4e-compose-new)
  :defer t

  :preface
  (progn
    (autoload 'message-insert-formatted-citation-line "message")
    (autoload 'message-send-mail-with-sendmail "message")

    (use-package cb-mu4e-utils
      :after mu4e
      :functions (cb-mu4e-utils-view-in-external-browser-action
                  cb-mu4e-utils-read-and-archive-action
                  mu4e-headers-mark-for-read-and-archive
                  mu4e-view-mark-for-read-and-archive)))

  :init
  (progn
    (with-eval-after-load 'org
      (require 'org-mu4e))

    (spacemacs-keys-set-leader-keys "am" #'mu4e))

  :config
  (progn
    (evilified-state-evilify-map mu4e-main-mode-map
      :mode mu4e-main-mode
      :bindings
      (kbd "j") #'mu4e~headers-jump-to-maildir)

    (evilified-state-evilify-map mu4e-headers-mode-map
      :mode mu4e-headers-mode
      :bindings
      (kbd "J") #'mu4e~headers-jump-to-maildir
      (kbd "j") #'mu4e-headers-next
      (kbd "k") #'mu4e-headers-prev)

    (evilified-state-evilify-map mu4e-view-mode-map
      :mode mu4e-view-mode
      :bindings
      (kbd "J") #'mu4e~view-headers-jump-to-maildir
      (kbd "n") #'mu4e-view-headers-next
      (kbd "p") #'mu4e-view-headers-prev
      (kbd "C-j") #'mu4e-view-headers-next
      (kbd "C-k") #'mu4e-view-headers-prev)

    ;; Set variables

    (setq mu4e-use-fancy-chars t)
    (setq mu4e-headers-attach-mark (purecopy '("a" . "A")))
    (setq mu4e-headers-unread-mark (purecopy '("u" . "●")))
    (setq mu4e-headers-seen-mark (purecopy '(" " . " ")))
    (setq mu4e-hide-index-messages t)

    (setq mu4e-view-prefer-html t)
    (setq mu4e-view-show-images t)
    (setq mu4e-view-show-addresses t)
    (setq message-kill-buffer-on-exit t)
    (setq mu4e-compose-signature-auto-include t)

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
            ("m:/walrus/inbox OR m:/movio/inbox"
             "All Inboxes" ?i)
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

    ;; use imagemagick, if available
    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    ;; Render html emails using something reasonable.
    (setq mu4e-html2text-command
          (cond
           ((executable-find "w3m")
            "w3m -dump -cols 80 -T text/html")
           ((executable-find "textutil")
            "textutil -stdin -format html -convert txt -stdout")
           (t
            'html2text)))

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
    (define-key mu4e-headers-mode-map (kbd "SPC") spacemacs-keys-default-map)))


(provide 'cb-mu4e)

;;; cb-mu4e.el ends here
