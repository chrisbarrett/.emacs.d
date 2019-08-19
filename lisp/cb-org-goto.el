;;; cb-org-goto.el --- Global org navigation commands.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'f)
(require 'org)

(autoload 'org-agenda-filter-apply "org-agenda")

(defvar cb-org-goto-on-holiday? nil
  "If nil, show the work agenda during work hours.")

;;;###autoload
(defun cb-org-goto-inbox ()
  "Switch to the inbox file."
  (interactive)
  (find-file (f-join org-directory "inbox.org")))

;;;###autoload
(defun cb-org-goto-notes ()
  "Switch to the default notes file."
  (interactive)
  (find-file org-default-notes-file))

;;;###autoload
(defun cb-org-goto-journal ()
  "Switch to the default notes file."
  (interactive)
  (find-file (f-join org-directory "journal.org")))

;;;###autoload
(defun cb-org-goto-work ()
  "Switch to the work file."
  (interactive)
  (find-file config-org-work-file))

;;;###autoload
(defun cb-org-goto-todo-list ()
  "Show the todo list."
  (interactive)
  (org-agenda prefix-arg "t")
  (org-agenda-filter-apply '("-someday") 'tag))

;;;###autoload
(defun cb-org-goto-tags-list ()
  "Show all tagged items."
  (interactive)
  (org-tags-view nil))

(defconst cb-org-goto--show-agenda-work-start-hour 8)
(defconst cb-org-goto--show-agenda-work-end-hour 17)

(defun cb-org-goto--is-work-time? (time)
  (-let* (((_s _m h d m y) time)
          (day-of-week (calendar-day-of-week (list m d y))))
    (and (<= cb-org-goto--show-agenda-work-start-hour h)
         (>= cb-org-goto--show-agenda-work-end-hour h)
         (<= 1 day-of-week)
         (>= 5 day-of-week))))

;;;###autoload
(defun cb-org-goto-headline ()
  "Prompt for a headline to jump to."
  (interactive)
  (let ((buf (if (derived-mode-p 'org-mode)
                 (current-buffer)
               (find-file-noselect org-default-notes-file))))
    (org-refile '(4) buf)))

(provide 'cb-org-goto)

;;; cb-org-goto.el ends here
