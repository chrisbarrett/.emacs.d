;;; org-funcs.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'f)

(autoload 'calendar-day-of-week "calendar")
(autoload 'org-get-deadline-time "org")
(autoload 'org-get-scheduled-time "org")
(autoload 'org-get-todo-state "org")
(autoload 'org-goto-sibling "org")
(autoload 'outline-next-heading "outline")

;; Silence byte-compiler.
(eval-when-compile
  (defvar org-directory nil))

(defun org-funcs-working-hours-p ()
  (-let* (((_s _m h d m y) (decode-time))
          (day-of-week (calendar-day-of-week (list m d y))))
    (and (<= 1 day-of-week 5)
         (or (<= 8 h 12) (<= 13 h 17)))))

(defun org-funcs-agenda-for-time-of-day ()
  "Show the org agenda for the time of day."
  (interactive)
  (if (org-funcs-working-hours-p)
      (org-agenda nil "w")
    (org-agenda nil "p")))

(defun org-funcs-exclude-tasks-on-hold (tag)
  (and (equal tag "hold") (concat "-" tag)))

(defun org-funcs-skip-item-if-timestamp ()
  "Skip the item if it has a scheduled or deadline timestamp."
  (when (or (org-get-scheduled-time (point))
            (org-get-deadline-time (point)))
    (or (outline-next-heading)
        (goto-char (point-max)))))

(defun org-funcs--current-headline-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(defun org-funcs-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-funcs--current-headline-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-funcs--current-headline-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-funcs-skip-duplicates-for-agenda ()
  (or (org-funcs-skip-item-if-timestamp)
      (org-funcs-agenda-skip-all-siblings-but-first)))

(cl-defun org-funcs-capture-template (key
                             label
                             form
                             template
                             &key
                             immediate-finish
                             jump-to-captured
                             (type 'entry)
                             (prepend t)
                             (clock-keep t))
  (list key label type form template
        :clock-keep clock-keep
        :prepend prepend
        :immediate-finish immediate-finish
        :jump-to-captured jump-to-captured))

(provide 'org-funcs)

;;; org-funcs.el ends here
