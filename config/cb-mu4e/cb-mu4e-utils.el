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
  (let ((browse-url-browser-function #'browse-url-default-browser))
    (mu4e-action-view-in-browser msg)))

(defun cb-mu4e-utils-read-and-archive-action (docid msg _target)
  ;; Must come before proc-move since retag runs 'sed' on the file
  (mu4e-action-retag-message msg "-\\Inbox")
  (-let (((_ . dest) (assoc 'mu4e-refile-folder (mu4e-context-vars (mu4e-context-current)))))
    (mu4e~proc-move docid dest "+S-u-N")))

(provide 'cb-mu4e-utils)

;;; cb-mu4e-utils.el ends here
