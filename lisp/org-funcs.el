;;; org-funcs.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash-functional)
(require 'f)
(require 'ht)
(require 'paths)
(require 'thingatpt)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-agenda)
  (require 'org-clock)
  (require 'org-capture))

(defvar org-agenda-files nil)
(defvar org-capture-templates nil)


;; Clocking related stuff
;;
;; Stolen from http://doc.norang.ca/org-mode.html#Clocking

(defun org-funcs-clocked-task-tags ()
  (when (marker-buffer org-clock-marker)
    (save-excursion
      (with-current-buffer (marker-buffer org-clock-marker)
        (org-get-tags-at (marker-position org-clock-marker))))))

(defun org-funcs-work-context-p ()
  (save-excursion
    (let ((tags (seq-map #'substring-no-properties
                         (append (ignore-errors (org-get-tags-at))
                                 (org-funcs-clocked-task-tags)))))
      (seq-contains tags "@work"))))

(defun org-funcs-agenda-dwim ()
  "Show the appropriate org agenda view."
  (interactive)
  (dolist (entry org-agenda-files)
    (cond ((file-regular-p entry)
           (find-file-noselect entry))
          ((file-directory-p entry)
           (dolist (file (f-files entry (lambda (it)
                                          (string-match-p org-agenda-file-regexp it))))
             (find-file-noselect file)))))
  (if (org-funcs-work-context-p)
      (org-agenda nil "wa")
    (org-agenda nil "pa")))

(defun org-funcs--ensure-default-datetree-entry (buffer)
  "Create the default heading for clocking in BUFFER.

Return the position of the headline."
  (save-excursion
    (org-with-point-at (org-find-exact-headline-in-buffer "Notes" buffer)
      (org-datetree-find-iso-week-create (calendar-current-date))
      (point))))

(defvar org-funcs--punching-in-p nil)
(defvar org-funcs--punching-out-p nil)

(defun org-funcs-work-file-buffer ()
  (find-file-noselect (f-join org-directory "work.org")))

(defun org-funcs-personal-file-buffer ()
  (find-file-noselect (f-join org-directory "work.org")))

(defun org-funcs-buffer-for-context ()
  (--find (equal (current-buffer) it)
          (list (org-funcs-personal-file-buffer)
                (org-funcs-work-file-buffer))))

(defun org-funcs-punch-in (buffer)
  "Punch in with the default date tree in the given BUFFER."
  (interactive (list (org-funcs-work-file-buffer)))
  (let ((org-funcs--punching-in-p t))
    (with-current-buffer buffer
      (save-excursion
        (if-let* ((pos (org-funcs--ensure-default-datetree-entry buffer)))
            (progn
              (goto-char pos)
              (org-clock-in '(16)))
          (error "Notes headline does not exist"))))))

(defun org-funcs-punch-out ()
  "Stop the clock."
  (interactive)
  (let ((org-funcs--punching-out-p t))
    (when (org-clock-is-active)
      (org-clock-out))
    (org-agenda-remove-restriction-lock)
    (message "Punched out.")))

(defun org-funcs-punch-in-or-out ()
  "Punch in or out of the current clock."
  (interactive)
  (call-interactively (if (org-clocking-p)
                          #'org-funcs-punch-out
                        #'org-funcs-punch-in)))

(defun org-funcs--at-default-task-p ()
  (when (equal (current-buffer)
               (marker-buffer org-clock-default-task))
    (save-excursion
      (org-back-to-heading t)
      (equal (point) (marker-position org-clock-default-task)))))

(defun org-funcs--clocking-on-default-task-p ()
  (ignore-errors
    (cl-labels ((org-marker-pos
                 (marker)
                 (org-with-point-at marker
                   (org-back-to-heading t)
                   (point))))
      (and (equal (marker-buffer org-clock-marker) (marker-buffer org-clock-default-task))
           (equal (org-marker-pos org-clock-marker)
                  (org-marker-pos org-clock-default-task))))))

(defun org-funcs--clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

;; Supporting functions for hooks.

(defun org-funcs-on-clock-on ()
  (unless org-funcs--punching-in-p
    (save-excursion
      (when-let* ((buf (org-funcs-buffer-for-context))
                  (pos (org-funcs--ensure-default-datetree-entry buf)))
        (org-clock-out nil 'no-error)
        (goto-char pos)
        (org-clock-mark-default-task)))))

(defun org-funcs-on-clock-out ()
  (unless org-funcs--punching-out-p
    (save-excursion
      (cond
       ((org-funcs--clocking-on-default-task-p)
        (org-clock-out))
       ((and (org-funcs-work-context-p) (not (org-funcs--at-default-task-p)))
        (org-funcs--clock-in-default-task))))))


;; Agenda utils

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

(cl-defun org-funcs-capture-template (key label form template &rest keywords)
  (let ((defaults '(:clock-keep t
                    :prepend t
                    :immediate-finish nil
                    :jump-to-captured nil)))
    (cl-list* key label 'entry form template (ht->plist (ht-merge
                                                         (ht-from-plist defaults)
                                                         (ht-from-plist keywords))))))

(defun org-funcs-todo-list ()
  "Show the todo list for the current context."
  (interactive)
  (org-agenda prefix-arg "t")
  (let ((tags (if (org-funcs-work-context-p)
                  '("-someday" "+@work")
                '("-someday" "-@work"))))
    (org-agenda-filter-apply tags 'tag)))



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
  (-let [(_ file _ pos)
         (org-refile-get-location "Goto"
                                  (when (derived-mode-p 'org-mode)
                                    (current-buffer)))]
    (find-file file)
    (widen)
    (cond
     (pos
      (goto-char pos)
      (org-narrow-to-subtree)
      (outline-hide-subtree)
      (org-show-entry)
      (org-show-children)
      (org-show-set-visibility 'canonical))
     (t
      (goto-char (point-min))
      (org-overview)
      (org-forward-heading-same-level 1)))))



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


;; Capture utils

(defun org-funcs--last-url-kill ()
  "Return the most recent URL in the kill ring or X pasteboard."
  (--first (s-matches? (rx bos (or "http" "https" "www")) it)
           (cons (current-kill 0 t) kill-ring)))

(defun org-funcs-read-url (prompt)
  (let ((default (or (thing-at-point-url-at-point) (org-funcs--last-url-kill))))
    (read-string (concat (if default (format "%s (default %s)" prompt default) prompt) ": ")
                 nil nil default)))

(defun org-funcs--decode-html-entities (str)
  (with-temp-buffer
    (insert str)
    (pcase (libxml-parse-html-region (point-min) (point-max))
      (`(html nil
              (body nil
                    (p nil
                       ,(and (pred stringp) decoded))))
       decoded))))

(defun org-funcs--parse-html-title (html)
  "Extract the title from an HTML document."
  (-let (((_ title) (s-match (rx "<title>" (group (* nonl)) "</title>") html))
         ((_ charset) (-map 'intern (s-match (rx "charset=" (group (+ (any "-" alnum)))) html))))
    (org-funcs--decode-html-entities (if (-contains? coding-system-list charset)
                                         (decode-coding-string title charset)
                                       title))))

(defun org-funcs--retrieve-html (url)
  (unless (s-matches? (rx "." (or "pdf" "mov" "mp4" "m4v" "aiff" "wav" "mp3") eol) url)
    (with-current-buffer (url-retrieve-synchronously url t)
      (buffer-string))))

(defun org-funcs-read-url-for-capture ()
  "Return a URL capture template string for use with `org-capture'."
  (let* ((url (org-funcs-read-url "URL"))
         (title (org-funcs--parse-html-title (org-funcs--retrieve-html url))))
    (format "* TODO Review [[%s][%s]]" url (or title url))))

(defun org-funcs-update-capture-templates (templates)
  "Merge TEMPLATES with existing values in `org-capture-templates'."
  (let ((ht (ht-merge (ht-from-alist org-capture-templates) (ht-from-alist templates))))
    (setq org-capture-templates (-sort (-on 'string-lessp 'car) (ht->alist ht)))))


;; Priorities

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



(defun org-funcs-refile-verify-function ()
  (let ((keyword (nth 2 (org-heading-components))))
    (not (member keyword org-done-keywords))))

(provide 'org-funcs)

;;; org-funcs.el ends here
