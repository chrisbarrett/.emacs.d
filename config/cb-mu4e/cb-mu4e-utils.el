;;; cb-mu4e-utils.el --- Supporting commands for mu4e config.

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'mu4e)


;; Expands to: mu4e-view-mark-for-read-and-archive
(mu4e~headers-defun-mark-for read-and-archive)

;; Expands to: mu4e-headers-mark-for-read-and-archive
(mu4e~view-defun-mark-for read-and-archive)

(defun cb-mu4e-utils-view-in-external-browser-action (msg)
  "View the current message MSG in the browser."
  (interactive (list mu4e~view-msg))
  (let ((browse-url-browser-function #'browse-url-default-browser))
    (mu4e-action-view-in-browser msg)))

(defun cb-mu4e-utils--refile-to-sent-maildir-p (sent-dir msg)
  (-let [(&plist :maildir message-dir :from ((_ . from-address))) msg]
    (or (equal message-dir sent-dir)
        (mu4e-user-mail-address-p from-address)
        (string-match-p (rx "@walrus.cool" eos) from-address))))

(defun cb-mu4e-utils-read-and-archive-action (docid msg _target)
  ;; Must come before proc-move since retag runs 'sed' on the file
  (mu4e-action-retag-message msg "-\\Inbox")
  ;; Move to refile dir, unless this is a sent message (which can happen if I
  ;; attempt to archive a thread).
  (let* ((refile-dir (mu4e-get-refile-folder msg))
         (sent-dir (mu4e-get-sent-folder msg))
         (target-dir (if (cb-mu4e-utils--refile-to-sent-maildir-p sent-dir msg) sent-dir refile-dir)))
    (mu4e~proc-move docid target-dir "+S-u-N")))

(provide 'cb-mu4e-utils)

;;; cb-mu4e-utils.el ends here
