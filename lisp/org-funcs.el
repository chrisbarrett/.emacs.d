;;; org-funcs.el --- Extra functions to support my orgmode configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Extra functions to support org-mode config.

;;; Code:

(require 'async)
(require 'dash)
(require 'f)
(require 'ht)
(require 'seq)
(require 'thingatpt)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-ref)
  (require 'org-roam)
  (require 'org-agenda)
  (require 'org-clock)
  (require 'org-capture))

(autoload 'timekeep-work-tag "timekeep")
(autoload 'org-cliplink-retrieve-title-synchronously "org-cliplink")
(autoload 'org-project-p "org-project")
(autoload 'org-project-skip-stuck-projects "org-project")
(autoload 'org-ref-url-html-to-bibtex "org-ref-url-utils")
(autoload 'xml-parse-string "xml")


;; Capture template helpers

(defun org-funcs-update-capture-templates (templates)
  "Merge TEMPLATES with existing values in `org-capture-templates'."
  (with-eval-after-load 'org-capture
    (let ((ht (ht-merge (ht-from-alist org-capture-templates) (ht-from-alist templates))))
      (setq org-capture-templates (-sort (-on 'string-lessp 'car) (ht->alist ht))))))

(defun org-funcs-capture-template-apply-defaults (template)
  (-let ((defaults '(:clock-keep t :prepend t :immediate-finish nil :jump-to-captured nil :empty-lines 1))
         ((positional-args keywords) (-split-with (-not #'keywordp) template)))
    (append positional-args (ht->plist (ht-merge
                                        (ht-from-plist defaults)
                                        (ht-from-plist keywords))))))

(cl-defun org-funcs-capture-template (key label form template &rest keywords)
  (org-funcs-capture-template-apply-defaults (append (list key label 'entry form template) keywords)))


;; Agenda utils

(defun org-funcs-agenda-dwim ()
  "Show the org agenda with appropriate tags set."
  (interactive)
  (if (org-clocking-p)
      (org-agenda nil "w")
    (org-agenda nil "p")))

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
  (equal "TODO" (org-get-todo-state)))

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



(defun org-funcs-goto-todos ()
  "Switch to the general life-admin todos file."
  (interactive)
  (switch-to-buffer (find-file (f-join org-directory "tasks" "todos.org"))))

(defun org-funcs-todo-list (tags)
  "Show the todo list for the current context.

TAGS are the tags to use when displaying the list."
  (interactive (list (if (org-clocking-p)
                         (list "-someday" "+work")
                       (list "-someday" "-work"))))
  (org-agenda prefix-arg "t")
  (org-agenda-filter-apply (cons "-ignore" tags) 'tag))

(defun org-funcs-goto-accounts ()
  "Goto the accounts file."
  (interactive)
  (org-roam-node-visit (org-roam-node-from-title-or-alias "Accounts")))


;; Capture utils

(defun org-funcs-capture-note-to-clocked-heading ()
  (unless (org-clocking-p)
    (user-error "No active clock"))
  (org-with-point-at org-clock-marker
    (org-add-note))
  "")

(defun org-funcs--last-url-kill ()
  "Return the most recent URL in the kill ring or X pasteboard."
  (--first (s-matches? (rx bos (or "http" "https" "www")) it)
           (cons (current-kill 0 t) kill-ring)))

(defun org-funcs-read-url (&optional prompt default)
  (let* ((default (-some->> (or default (thing-at-point-url-at-point) (org-funcs--last-url-kill))
                    (string-trim)))
         (prompt (or prompt "URL"))
         (input (read-string (concat (if default (format "%s (default %s)" prompt default) prompt) ": ")
                             nil nil default)))
    (substring-no-properties
     (if (string-match-p (rx "http" (? "s") "://") input)
         input
       (org-funcs-read-url prompt default)))))

(defun org-funcs-guess-or-retrieve-title (url)
  (if-let* ((match (s-match (rx "github.com/" (group (+? nonl) (or "/issues/" "/pull/") (+ digit) eol))
                            url)))
      (cadr match)
    (org-cliplink-retrieve-title-synchronously url)))

(defun org-funcs-insert-url-as-link (url)
  "Insert an orgmode link at point for URL."
  (interactive (list (org-funcs-read-url)))
  (save-match-data
    (let* ((title (org-funcs-guess-or-retrieve-title url))
           (escaped-title (s-replace-all '(("[" . "(")
                                           ("]" . ")"))
                                         title)))
      (unless (thing-at-point-looking-at (rx bol (* space)))
        (just-one-space))
      (insert (format "[[%s][%s]]" url escaped-title))
      (just-one-space))))

(defconst org-funcs--wkhtmltopdf-error-buffer-name "*wkhtmltopdf errors*")

(defun org-funcs--update-wkhtmltopdf-error-buffer (output)
  (with-current-buffer (get-buffer-create org-funcs--wkhtmltopdf-error-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert output))
    (read-only-mode +1)
    (current-buffer)))

(defvar org-funcs-wkhtmltopdf-program "wkhtmltopdf")

(defun org-funcs--key-of-latest-bib-entry (bibfile)
  (with-current-buffer (find-file bibfile)
    (save-excursion
      (goto-char (point-max))
      (bibtex-beginning-of-entry)
      (let ((bibtex-expand-strings t))
        (reftex-get-bib-field "=key=" (bibtex-parse-entry t))))))

(defvar org-funcs--pdf-download-timeout-seconds 30)

(defun org-funcs-url-to-reference (url &optional show-pdf)
  "Create a PDF of URL and add it to the bibliography.

Optional argument SHOW-PDF determines whether to show the downloaded PDF."
  (interactive (list (org-funcs-read-url "Add reference to URL")
                     t))
  (require 'org-ref)
  (let* ((url (car (split-string url "?")))
         (bibfile (car (org-ref-find-bibliography)))
         (tmpfile (make-temp-file "wkhtmltopdf_" nil ".pdf"))
         (reporter (make-progress-reporter "Downloading PDF"))
         (status)
         (process
          (async-start-process "wkhtmltopdf"
                               org-funcs-wkhtmltopdf-program
                               (lambda (_proc)
                                 (setq status 'done))
                               "--log-level" "warn"
                               url
                               tmpfile))
         (buf (process-buffer process)))

    (cl-labels ((cleanup-on-error ()
                                  (progress-reporter-done reporter)
                                  (let ((cause (with-current-buffer buf (buffer-string))))
                                    (org-funcs--update-wkhtmltopdf-error-buffer cause))
                                  (ignore-errors
                                    (delete-file tmpfile))
                                  (ignore-errors
                                    (kill-buffer (process-buffer process))))
                (go ()
                    (pcase-exhaustive status
                      ('timeout
                       (cleanup-on-error)
                       (message "PDF creation timed out. See %s for details." org-funcs--wkhtmltopdf-error-buffer-name))
                      ((guard (process-live-p process))
                       (progress-reporter-update reporter)
                       (run-with-timer 0.5 nil #'go))
                      ((or 'done (guard (< 0 (f-size tmpfile))))
                       (progress-reporter-done reporter)
                       ;; Append an entry to the bibfile.
                       (org-ref-url-html-to-bibtex bibfile url)
                       (let* ((key (org-funcs--key-of-latest-bib-entry bibfile))
                              (target (f-join org-ref-pdf-directory (format  "%s.pdf" key))))
                         (rename-file tmpfile target)
                         (when show-pdf
                           (find-file target))
                         (message "PDF downloaded to %s" target)))
                      (_
                       (cleanup-on-error)
                       (message "PDF download failed. See %s for details." org-funcs--wkhtmltopdf-error-buffer-name)))))
      (go))

    (run-with-timer org-funcs--pdf-download-timeout-seconds
                    nil
                    (lambda ()
                      (setq status 'timeout)))))

(defun org-funcs--file-tags ()
  (save-match-data
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (search-forward-regexp (rx bol "#+filetags:" (group (+ nonl)))
                                  nil
                                  t)
       (split-string (string-trim (substring-no-properties (match-string 1))) ":" t)))))

(defun org-funcs--set-file-tags (tags)
  (org-with-wide-buffer
   (goto-char (point-min))
   (unless (search-forward-regexp (rx bol "#+filetags:" (group (* nonl))) nil t)
     (cond ((search-forward-regexp (rx bol "#+title:"))
            (goto-char (line-end-position))
            (insert "\n#+filetags:"))
           (t
            (insert "#+filetags:\n"))))

   (let ((formatted (if tags
                        (format ":%s:" (string-join tags ":"))
                      "")))
     (save-match-data
       (goto-char (point-min))
       (when (search-forward-regexp (rx bol "#+filetags:" (group (* nonl))))
         (replace-region-contents (match-beginning 1) (match-end 1)
                                  (lambda ()
                                    (concat " " formatted))))))))

(defun org-funcs-roam-extract-subtree ()
  "Convert current subtree at point to a node, and extract it into a new file.

It's a re-implementation of `org-roam-extract-subtree', but
handles file titles, IDs and tags better."
  (interactive)
  (let ((dest-file (expand-file-name (format-time-string "%Y-%M-%d--%H-%M-%S.org"))))
    (save-excursion
      (org-back-to-heading-or-point-min t)
      (when (bobp) (user-error "Already a top-level node"))

      (save-buffer)
      (org-roam-db-update-file)
      (when (file-exists-p dest-file)
        (user-error "%s exists. Aborting" dest-file))

      (let ((title (org-get-heading))
            (tags (org-get-tags))
            (id (org-id-get-create)))

        (atomic-change-group
          (org-cut-subtree)
          (insert (org-link-make-string (format "id:%s" id) title))
          (newline)
          (save-buffer)
          (with-current-buffer (find-file-noselect dest-file)
            (org-paste-subtree)
            (org-roam-promote-entire-buffer)
            (when-let* ((tags (-union (org-funcs--file-tags) tags)))
              (org-funcs--set-file-tags tags))
            (save-buffer)))))))


(defun org-funcs-read-tags-filter (&optional default)
  (let* ((input (read-string "[+-]Tags: " default 'org-funcs-read-tags-filter-history))
         (tags (split-string input (rx (+ (any space ":"))) t)))
    (seq-reduce (lambda (acc tag)
                  (if (string-prefix-p "-" tag)
                      (push (s-chop-prefix "-" tag) (plist-get acc :exclude))
                    (push (s-chop-prefix "+" tag) (plist-get acc :include)))
                  acc)
                tags
                '(:include nil
                  :exclude nil))))

(defun org-funcs-roam-node-find (&optional arg)
  "Find an org-roam node. See `org-roam-node-find'.

With optional prefix ARG, prompt for a tags filter."
  (interactive "P")
  (let ((filter
         (if arg
             (-let [(&plist :include :exclude) (org-funcs-read-tags-filter "-dailies ")]
               (lambda (node)
                 (let ((tags (org-roam-node-tags node)))
                   (and (if exclude (null (seq-intersection tags exclude)) t)
                        (if include (seq-intersection tags include) t)))))
           (lambda (node)
             (not (seq-contains-p (org-roam-node-tags node) "dailies"))))))
    (org-roam-node-find arg nil filter)))



(defun org-funcs-fix-blank-lines (&optional scope)
  "Ensure that blank lines exist between headings and their contents.

SCOPE is as defined by `org-map-entries'."
  (interactive (list (if (equal (buffer-name) "archive.org") 'tree 'file)))
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (delete-blank-lines)
                       (when (and (eobp) (not (thing-at-point-looking-at "\n")))
                         (insert "\n"))))
                   t
                   scope))

(provide 'org-funcs)

;;; org-funcs.el ends here
