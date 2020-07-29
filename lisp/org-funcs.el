;;; org-funcs.el --- Extra functions to support my orgmode configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'alert)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 'ht)
(require 'paths)
(require 'thingatpt)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-roam)
  (require 'org-agenda)
  (require 'org-clock)
  (require 'org-capture))

(autoload 'org-project-p "org-project")
(autoload 'org-project-skip-stuck-projects "org-project")
(autoload 'xml-parse-string "xml")

(defvar org-agenda-files nil)
(defvar org-capture-templates nil)
(defvar org-agenda-custom-commands nil)

(defun org-funcs-dailies-file-path ()
  (let* ((template (alist-get "d" org-roam-dailies-capture-templates nil nil #'equal))
         (file-path-template (plist-get template :file-name))
         (expanded-path (concat (string-trim (org-roam-capture--fill-template file-path-template))
                                ".org")))
    (f-join org-roam-directory expanded-path)))

(defun org-funcs-dailies-buffer-get-create ()
  (let ((path (org-funcs-dailies-file-path)))
    (with-current-buffer
        (or (get-file-buffer path)
            (find-file-noselect path))
      (save-restriction
        (widen)
        (when (= (buffer-size) 0)
          ;; Insert default content for dailies file.
          (let* ((template (alist-get "d" org-roam-dailies-capture-templates nil nil #'equal))
                 (content-template (plist-get template :head)))
            (insert (org-roam-capture--fill-template content-template)))))

      (current-buffer))))

(defun org-funcs-dailies-today ()
  "Switch to today's notes file.

Slightly faster than the version that ships with org-roam."
  (interactive)
  (switch-to-buffer (org-funcs-dailies-buffer-get-create)))


;; Clocking related stuff
;;
;; Stolen from http://doc.norang.ca/org-mode.html#Clocking

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
  (if (org-clocking-p)
      (org-agenda nil "wa")
    (org-agenda nil "pa"))
  (get-buffer org-agenda-buffer-name))


(defvar org-funcs--clocking-heading "Work hours")

(defun org-funcs--ensure-clocking-headline (buffer)
  "Create the default heading for clocking in BUFFER.

Return the position of the headline."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (if-let* ((marker (org-find-exact-headline-in-buffer org-funcs--clocking-heading)))
            (marker-position marker)
          (goto-char (point-max))
          (delete-horizontal-space)
          (org-insert-heading nil nil t)
          (insert org-funcs--clocking-heading)
          (org-set-tags "@work")
          (point))))))

(defun org-funcs-punch-in (buffer)
  "Punch in with the default date tree in the given BUFFER."
  (interactive (list (org-funcs-dailies-buffer-get-create)))
  (with-current-buffer buffer
    (org-with-point-at (org-funcs--ensure-clocking-headline buffer)
      (org-clock-in '(16))))
  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (org-funcs-agenda-dwim)))

(defun org-funcs-punch-out ()
  "Stop the clock."
  (interactive)
  (when (org-clocking-p)
    (org-clock-out))
  (org-agenda-remove-restriction-lock)
  (with-current-buffer (org-funcs-dailies-buffer-get-create)
    (save-buffer))
  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda for context change.
    (org-funcs-agenda-dwim))
  (message "Punched out."))

(defun org-funcs-punch-in-or-out ()
  "Punch in or out of the current clock."
  (interactive)
  (call-interactively (if (org-clocking-p)
                          #'org-funcs-punch-out
                        #'org-funcs-punch-in)))


;; Agenda utils

(defun org-funcs-exclude-tasks-on-hold (tag)
  (and (equal tag "hold") (concat "-" tag)))

(defun org-funcs--scheduled-or-deadline-p ()
  (or (org-get-scheduled-time (point))
      (org-get-deadline-time (point))))

(defun org-funcs--skip-heading-safe ()
  (or (outline-next-heading)
      (goto-char (point-max))))

(defun org-funcs-skip-item-if-timestamp ()
  "Skip the item if it has a scheduled or deadline timestamp."
  (when (org-funcs--scheduled-or-deadline-p)
    (org-funcs--skip-heading-safe)))

(defun org-funcs--current-headline-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(defun org-funcs--first-todo-at-this-level-p ()
  (let (should-skip-entry)
    (unless (org-funcs--current-headline-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-funcs--current-headline-is-todo)
          (setq should-skip-entry t))))
    should-skip-entry))

(defun org-funcs-high-priority-p ()
  (equal ?A (nth 3 (org-heading-components))))

(defun org-funcs--parent-scheduled-in-future-p ()
  (save-restriction
    (widen)
    (save-excursion
      (let ((found)
            (now (current-time)))
        (while (and (not found) (org-up-heading-safe))
          (when-let* ((scheduled (org-get-scheduled-time (point) t)))
            (when (time-less-p now scheduled)
              (setq found t))))
        found))))

(defun org-funcs-skip-items-already-in-agenda ()
  (cond
   ;; Don't show things that will naturally show in the agenda.
   ((or (org-funcs--scheduled-or-deadline-p) (org-funcs--parent-scheduled-in-future-p))
    (org-funcs--skip-heading-safe))

   ((and (org-funcs-high-priority-p) (org-funcs--current-headline-is-todo))
    ;; Show these items.
    nil)

   ((org-project-p)
    (org-project-skip-stuck-projects))

   ((org-funcs--first-todo-at-this-level-p)
    (org-funcs--skip-heading-safe))))



(defun org-funcs-goto-inbox ()
  "Switch to the inbox file."
  (interactive)
  (find-file (f-join paths-org-directory "inbox.org")))

(defun org-funcs-get-roam-file-by-title (title)
  (cl-labels ((extract-title (record) (plist-get (cdr record) :title))
              (extract-file (record) (plist-get (cdr record) :path)))
    (if-let* ((entries (org-roam--get-title-path-completions))
              (hit (seq-find (lambda (it) (equal title (extract-title it))) entries)))
        (extract-file hit)
      (error "No roam files with the given title"))))

(defvar org-funcs-work-file-title nil)

(defun org-funcs-goto-work ()
  "Switch to the work file."
  (interactive)
  (find-file (org-funcs-get-roam-file-by-title org-funcs-work-file-title))
  (org-show-all))

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
      (let ((org-indirect-buffer-display 'current-window))
        (org-tree-to-indirect-buffer))
      (outline-hide-subtree)
      (org-show-entry)
      (org-show-children)
      (org-show-set-visibility 'canonical))
     (t
      (goto-char (point-min))
      (org-overview)
      (org-forward-heading-same-level 1)))))

(defun org-funcs-todo-list ()
  "Show the todo list for the current context."
  (interactive)
  (org-agenda prefix-arg "t")
  (let ((tags (if (org-clocking-p)
                  '("-someday" "+@work")
                '("-someday" "-@work"))))
    (org-agenda-filter-apply tags 'tag)))



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

(defun org-funcs-read-url (&optional prompt default)
  (let* ((default (or default (thing-at-point-url-at-point) (org-funcs--last-url-kill)))
         (prompt (or prompt "URL"))
         (input (read-string (concat (if default (format "%s (default %s)" prompt default) prompt) ": ")
                             nil nil default)))
    (if (string-match-p (rx "http" (? "s") "://") input)
        input
      (org-funcs-read-url prompt default))))

(defun org-funcs--retrieve-html (url)
  (with-current-buffer (url-retrieve-synchronously url t)
    (goto-char (point-min))
    ;; Remove DOS EOL chars
    (while (search-forward "\r\n" nil t)
      (replace-match "\n"))

    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (libxml-parse-html-region (point) (point-max))))

(defun org-funcs--unencode-entities-in-string (str)
  (or (ignore-errors
        (with-temp-buffer
          (insert str)
          (goto-char (point-min))
          (xml-parse-string)))
      str))

(defun org-funcs--guess-title-from-url-fragment (url)
  (-some->> (url-generic-parse-url url)
    (url-filename)
    (f-filename)
    (url-unhex-string)
    (s-replace "+" " " )))

(defun org-funcs--extract-title (html)
  (cadr (alist-get 'title (cdr (alist-get 'head (cdr html))))))

(defun org-funcs-guess-or-retrieve-title (url)
  (cond
   ((string-match-p (rx ".atlassian.net/wiki/") url)
    (org-funcs--guess-title-from-url-fragment url))
   ((string-match-p (rx "github.com/" (+? nonl) "/pull/" (+ digit) eol) url)
    (cadr (s-match (rx "github.com/" (group (+? nonl) "/pull/" (+ digit) eol))
                   url)))
   (t
    (-some->> (org-funcs--retrieve-html url)
      (org-funcs--extract-title)
      (s-replace-regexp (rx (any "\r\n\t")) "")
      (s-trim)))))

(defconst org-funcs--domain-to-verb-alist
  '(("audible.com" . "Listen to")
    ("goodreads.com" . "Read")
    ("netflix.com" . "Watch")
    ("vimeo.com" . "Watch")
    ("youtube.com" . "Watch")))

(defun org-funcs--parse-github-repo-from-url (url)
  (cadr (s-match (rx "github.com/" (group (+? nonl) "/" (+ (not (any "/?")))))
                 url)))

(defvar org-funcs-work-repos nil)

(defun org-funcs--work-related-url-p (url)
  (let ((host (url-host (url-generic-parse-url url))))
    (or
     (string-match-p (rx (or "atlassian.com"))
                     host)
     (when-let* ((repo (org-funcs--parse-github-repo-from-url url)))
       (seq-contains-p org-funcs-work-repos repo)))))

(defun org-funcs-read-url-for-capture (&optional url title notify-p)
  "Return a URL capture template string for use with `org-capture'.

URL and TITLE are added to the template.

If NOTIFY-P is set, a desktop notification is displayed."
  (interactive
   (let* ((url (org-funcs-read-url))
          (guess (org-funcs-guess-or-retrieve-title url))
          (title (read-string "Title: " guess)))
     (list url title nil)))

  (let* ((domain (string-remove-prefix "www." (url-host (url-generic-parse-url url))))
         (verb (alist-get domain org-funcs--domain-to-verb-alist "Review" nil #'equal))
         (tags (if (org-funcs--work-related-url-p url) ":@work:" "")))
    (prog1
        (format "* TODO %s [[%s][%s]]     %s"
                verb
                url
                (org-link-escape (or title url))
                tags)
      (when notify-p
        (alert title :title "Link Captured")))))

(defun org-funcs-capture-link ()
  "Context-sensitive link capture."
  (if (derived-mode-p 'mu4e-view-mode 'mu4e-headers-mode)
      (progn
        (org-store-link nil)
        "* TODO Review %a (email)")
    (call-interactively #'org-funcs-read-url-for-capture)))

(defun org-funcs-dailies-file-for-capture ()
  (switch-to-buffer (org-funcs-dailies-buffer-get-create))
  (goto-char (point-max)))

(defun org-funcs-capture-todo ()
  (let ((tags (if (org-clocking-p) ":@work:" "")))
    (concat "* TODO %?           " tags)))

(defun org-funcs-update-capture-templates (templates)
  "Merge TEMPLATES with existing values in `org-capture-templates'."
  (let ((ht (ht-merge (ht-from-alist org-capture-templates) (ht-from-alist templates))))
    (setq org-capture-templates (-sort (-on 'string-lessp 'car) (ht->alist ht)))))

(defun org-funcs-capture-template-apply-defaults (template)
  (-let ((defaults '(:clock-keep t :prepend t :immediate-finish nil :jump-to-captured nil :empty-lines 1))
         ((positional-args keywords) (-split-with (-not #'keywordp) template)))
    (append positional-args (ht->plist (ht-merge
                                        (ht-from-plist defaults)
                                        (ht-from-plist keywords))))))

(cl-defun org-funcs-capture-template (key label form template &rest keywords)
  (org-funcs-capture-template-apply-defaults (append (list key label 'entry form template) keywords)))

(defun org-funcs-update-agenda-custom-commands (templates)
  (let ((ht (ht-merge (ht-from-alist org-agenda-custom-commands) (ht-from-alist templates))))
    (setq org-agenda-custom-commands (-sort (-on 'string-lessp 'car) (ht->alist ht)))))


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



(defun org-funcs-refile-dwim ()
  "Run the correct refile command for the current buffer."
  (interactive)
  (call-interactively (cond
                       ((bound-and-true-p org-agenda-mode)
                        #'org-agenda-refile)
                       ((bound-and-true-p org-capture-mode)
                        #'org-capture-refile)
                       (t
                        #'org-refile))))

(defconst org-funcs--refile-ignored-filename-regexp
  (rx (or "archive"
          "dailies"
          "templates"
          "conflicted copy")))

(defun org-funcs-refile-candidates ()
  (f-files org-directory
           (lambda (it)
             (and (string-match-p "\\.org\\'" it)
                  (not (string-match-p org-funcs--refile-ignored-filename-regexp it))))
           t))

(defun org-funcs-refile-verify-function ()
  (let ((keyword (nth 2 (org-heading-components))))
    (not (member keyword org-done-keywords))))

(provide 'org-funcs)

;;; org-funcs.el ends here
