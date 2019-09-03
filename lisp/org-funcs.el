;;; org-funcs.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'f)
(require 'paths)

(eval-when-compile
  (require 'org nil t)
  (require 'org-agenda nil t)
  (require 'org-capture nil t))

(autoload 'calendar-day-of-week "calendar")
(autoload 'outline-next-heading "outline")
(autoload 'thing-at-point-url-at-point "thingatpt")

(defun org-funcs-working-hours-p ()
  (-let* (((_s _m h d m y) (decode-time))
          (day-of-week (calendar-day-of-week (list m d y))))
    (and (<= 1 day-of-week 5)
         (or (< 8 h 12) (<= 13 h 17)))))

(defun org-funcs-agenda-for-time-of-day ()
  "Show the org agenda for the time of day."
  (interactive)
  (if (org-funcs-working-hours-p)
      (org-agenda nil "wa")
    (org-agenda nil "pa")))

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

(defun org-funcs-high-priority-p ()
  (equal ?A (nth 3 (org-heading-components))))

(defun org-funcs-skip-items-already-in-agenda ()
  (or
   (org-funcs-skip-item-if-timestamp)
   (if (and (org-funcs-high-priority-p) (org-funcs--current-headline-is-todo))
       nil
     (org-funcs-agenda-skip-all-siblings-but-first))))

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

(defun org-funcs-todo-list ()
  "Show the todo list."
  (interactive)
  (org-agenda prefix-arg "t")
  (org-agenda-filter-apply '("-someday") 'tag))



(defun org-funcs-goto-inbox ()
  "Switch to the inbox file."
  (interactive)
  (find-file (f-join paths-org-directory "inbox.org")))

(defun org-funcs-goto-personal ()
  "Switch to the personal notes file."
  (interactive)
  (find-file (f-join paths-org-directory "personal.org")))

(defun org-funcs-goto-notes ()
  "Switch to the default notes file."
  (interactive)
  (require 'org)
  (find-file (f-join paths-org-directory org-default-notes-file)))

(defun org-funcs-goto-work ()
  "Switch to the work file."
  (interactive)
  (find-file (f-join paths-org-directory "work.org")))

(defun org-funcs-goto-headline ()
  "Prompt for a headline to jump to."
  (interactive)
  (org-refile '(4) (when (derived-mode-p 'org-mode)
                     (current-buffer))))

(defun org-funcs-ctrl-c-ctrl-k ()
  "Kill subtrees, unless we're in a special buffer where it should cancel."
  (interactive)
  (cond
   ((and (boundp 'org-capture-mode) org-capture-mode)
    (org-capture-kill))
   ((s-starts-with? "*Org" (buffer-name))
    (org-kill-note-or-show-branches))
   (t
    (org-cut-subtree))))

(defun org-funcs-ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-todo-heading'."
  (interactive)
  (if (org-at-table-p)
      (call-interactively #'org-table-hline-and-move)
    (call-interactively #'org-insert-todo-heading)))



(defun org-funcs--last-url-kill ()
  "Return the most recent URL in the kill ring or X pasteboard."
  (--first (s-matches? (rx bos (or "http" "https" "www")) it)
           (cons (current-kill 0 t) kill-ring)))

(defun org-funcs-read-url (prompt)
  (let ((default (or (thing-at-point-url-at-point) (org-funcs--last-url-kill))))
    (read-string (concat (if default (format "%s (default %s)" prompt default) prompt) ": ")
                 nil nil default)))



(defun org-funcs-toggle-priority ()
  "Toggle the priority cookie on the current line."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (-let [(_ _ _ priority) (org-heading-components)]
      (cond (priority
             (org-priority ?\s)
             (message "Priority cleared"))
            (t
             (org-priority ?A)
             (message "Priority set"))))))

(defun org-funcs-agenda-toggle-priority ()
  "Toggle the priority cookie on the current line."
  (interactive)

  (org-agenda-check-no-diary)
  (unless (org-get-at-bol 'org-marker)
    (org-agenda-error))

  (let* ((col (current-column))
         (heading-marker (org-get-at-bol 'org-hd-marker))
         (buffer (marker-buffer heading-marker))
         (pos (marker-position heading-marker))
         (inhibit-read-only t)
         updated-heading)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (org-funcs-toggle-priority)
        (setq updated-heading (org-get-heading)))
      (org-agenda-change-all-lines updated-heading heading-marker)
      (org-move-to-column col))))

(provide 'org-funcs)

;;; org-funcs.el ends here
