;;; cb-mu4e-utils.el --- Supporting commands for mu4e config.

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 'mu4e)

(defun cb-mu4e-utils-view-in-external-browser-action (msg)
  "View the current message MSG in the browser."
  (interactive (list mu4e~view-msg))
  (let ((browse-url-browser-function #'browse-url-default-browser))
    (mu4e-action-view-in-browser msg)))

(defun cb-mu4e-utils--refile-to-sent-maildir-p (sent-dir msg)
  (-let [(&plist :maildir message-dir :from ((_ . from-address))) msg]
    (equal message-dir sent-dir)))

(defun cb-mu4e-utils--refile-to-notifications-maildir-p (notifications-dir msg)
  (-let [(&plist :maildir message-dir) msg]
    (equal message-dir notifications-dir)))

(defun cb-mu4e-utils--from-me-p (msg)
  (-let [(&plist :maildir message-dir :from ((_ . from-address))) msg]
    (or (mu4e-user-mail-address-p from-address)
        (string-match-p (rx "@walrus.cool" eos) from-address))))

(defun cb-mu4e-utils--select-target-dir-for-refile (msg)
  ;; Move to refile dir, unless:
  ;; - this is a sent message (which can happen if I attempt to archive a thread).
  ;; - this is a notification
  ;; - this is a message to myself (it's already in sent messages, and can be trashed)
  (let* ((refile-dir (mu4e-get-refile-folder msg))
         (notifications-dir (f-expand "../notifications" refile-dir))
         (sent-dir (mu4e-get-sent-folder msg)))
    (cond
     ((cb-mu4e-utils--refile-to-sent-maildir-p sent-dir msg)
      sent-dir)
     ((cb-mu4e-utils--from-me-p msg)
      (mu4e-get-trash-folder msg))
     ((cb-mu4e-utils--refile-to-notifications-maildir-p notifications-dir msg)
      notifications-dir)
     (t
      refile-dir))))

(defun cb-mu4e-utils-read-and-archive-action (docid msg target)
  ;; Retag must come before proc-move since retag runs 'sed' on the file
  (mu4e-action-retag-message msg "-\\Inbox")
  (mu4e~proc-move docid target "+S-u-N"))

(provide 'cb-mu4e-utils)

;;; cb-mu4e-utils.el ends here
