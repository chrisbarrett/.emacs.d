;;; cb-org-goto.el --- Global org navigation commands.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'f)
(require 'org)

(defvar cb-org-goto-on-holiday? nil
  "If nil, show the work agenda during work hours.")

;;;###autoload
(defun cb-org-goto-inbox ()
  "Switch to the inbox file."
  (interactive)
  (find-file (f-join org-directory "inbox.org")))

;;;###autoload
(defun cb-org-goto-personal ()
  "Switch to the personal notes file."
  (interactive)
  (find-file (f-join org-directory "personal.org")))

;;;###autoload
(defun cb-org-goto-notes ()
  "Switch to the default notes file."
  (interactive)
  (find-file (f-join org-directory org-default-notes-file)))

;;;###autoload
(defun cb-org-goto-journal ()
  "Switch to the default notes file."
  (interactive)
  (find-file (f-join org-directory "journal.org")))

;;;###autoload
(defun cb-org-goto-work ()
  "Switch to the work file."
  (interactive)
  (find-file (f-join org-directory "work.org")))

;;;###autoload
(defun cb-org-goto-headline ()
  "Prompt for a headline to jump to."
  (interactive)
  (org-refile '(4) (when (derived-mode-p 'org-mode)
                     (current-buffer))))

(provide 'cb-org-goto)

;;; cb-org-goto.el ends here
