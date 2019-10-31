;;; mu4e-refile.el --- Utilities for refiling messages.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'f)
(require 'mu4e)

(defun mu4e-refile--refile-to-sent-maildir-p (sent-dir msg)
  (-let [(&plist :maildir message-dir :from ((_ . _from-address))) msg]
    (equal message-dir sent-dir)))

(defun mu4e-refile--refile-to-notifications-maildir-p (notifications-dir msg)
  (-let [(&plist :maildir message-dir) msg]
    (equal message-dir notifications-dir)))

(defun mu4e-refile--from-me-p (msg)
  (-let [(&plist :maildir _message-dir :from ((_ . from-address))) msg]
    (or (mu4e-user-mail-address-p from-address)
        (string-match-p (rx "@walrus.cool" eos) from-address))))

(defun mu4e-refile--select-target-dir-for-refile (msg)
  ;; Move to refile dir, unless:
  ;; - this is a sent message (which can happen if I attempt to archive a thread).
  ;; - this is a notification
  ;; - this is a message to myself (it's already in sent messages, and can be trashed)
  (let* ((refile-dir (mu4e-get-refile-folder msg))
         (notifications-dir (f-expand "../Notifications" refile-dir))
         (sent-dir (mu4e-get-sent-folder msg)))
    (cond
     ((mu4e-refile--refile-to-sent-maildir-p sent-dir msg)
      sent-dir)
     ((mu4e-refile--from-me-p msg)
      (mu4e-get-trash-folder msg))
     ((mu4e-refile--refile-to-notifications-maildir-p notifications-dir msg)
      notifications-dir)
     (t
      refile-dir))))

(defun mu4e-refile-read-and-archive-action (docid msg target)
  ;; Retag must come before proc-move since retag runs 'sed' on the file
  (mu4e-action-retag-message msg "-\\Inbox")
  (mu4e~proc-move docid target "+S-u-N"))

(provide 'mu4e-refile)

;;; mu4e-refile.el ends here
