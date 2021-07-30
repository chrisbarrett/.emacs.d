;;; org-funcs.el --- Extra functions to support my orgmode configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Extra functions to support org-mode config.

;;; Code:

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

(autoload 'clocking-work-tag "clocking")
(autoload 'org-cliplink-retrieve-title-synchronously "org-cliplink")
(autoload 'org-project-p "org-project")
(autoload 'org-project-skip-stuck-projects "org-project")
(autoload 'org-ref-url-html-to-bibtex "org-ref-url-utils")
(autoload 'xml-parse-string "xml")


;; Clocking related stuff
;;
;; Stolen from http://doc.norang.ca/org-mode.html#Clocking

(defconst org-funcs-custom-command-key "A")

(defun org-funcs-agenda-dwim ()
  "Show the org agenda with appropriate tags set."
  (interactive)
  (dolist (entry org-agenda-files)
    (cond ((file-regular-p entry)
           (find-file-noselect entry))
          ((file-directory-p entry)
           (dolist (file (f-files entry (lambda (it)
                                          (string-match-p org-agenda-file-regexp it))))
             (find-file-noselect file)))))
  (let ((org-agenda-tag-filter-preset (-union org-agenda-tag-filter-preset
                                              (if (org-clocking-p)
                                                  (list (format "+%s" (clocking-work-tag)))
                                                '("-work")))))
    (org-agenda nil org-funcs-custom-command-key))
  (get-buffer org-agenda-buffer-name))


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

(defun org-funcs-guess-or-retrieve-title (url)
  (if-let* ((match (s-match (rx "github.com/" (group (+? nonl) (or "/issues/" "/pull/") (+ digit) eol))
                            url)))
      (cadr match)
    (org-cliplink-retrieve-title-synchronously url)))

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

(defun org-funcs-read-url-for-capture (&optional url title)
  "Return a URL capture template string for use with `org-capture'.

URL and TITLE are added to the template.

If NOTIFY-P is set, a desktop notification is displayed."
  (interactive
   (let* ((url (org-funcs-read-url))
          (guess (org-funcs-guess-or-retrieve-title url))
          (title (read-string "Title: " guess)))
     (list url title)))

  (let* ((domain (string-remove-prefix "www." (url-host (url-generic-parse-url url))))
         (verb (alist-get domain org-funcs--domain-to-verb-alist "Review" nil #'equal))
         (tags (if (org-funcs--work-related-url-p url) (format ":%s:" (clocking-work-tag)) "")))
    (format "* TODO %s [[%s][%s]]     %s"
            verb
            url
            (org-link-escape (or title url))
            tags)))

(defun org-funcs-capture-link ()
  "Context-sensitive link capture."
  (if (derived-mode-p 'mu4e-view-mode 'mu4e-headers-mode)
      (progn
        (org-store-link nil)
        "* TODO Review %a (email)")
    (call-interactively #'org-funcs-read-url-for-capture)))

(defun org-funcs-update-agenda-custom-commands (templates)
  (with-eval-after-load 'org-agenda
    (let ((ht (ht-merge (ht-from-alist org-agenda-custom-commands) (ht-from-alist templates))))
      (setq org-agenda-custom-commands (-sort (-on 'string-lessp 'car) (ht->alist ht))))))

(defvar org-funcs--pdf-download-timeout-seconds 30)

(defconst org-funcs--wkhtmltopdf-error-buffer-name "*wkhtmltopdf errors*")

(defun org-funcs--update-wkhtmltopdf-error-buffer (output)
  (with-current-buffer (get-buffer-create org-funcs--wkhtmltopdf-error-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert output))
    (read-only-mode +1)
    (current-buffer)))

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
                               "/usr/bin/wkhtmltopdf"
                               (lambda (_proc)
                                 (setq status 'done))
                               "--log-level" "warn"
                               url
                               tmpfile))
         (buf (process-buffer process)))

    (cl-labels ((key-of-latest-bib-entry ()
                                         (with-current-buffer (find-file bibfile)
                                           (save-excursion
                                             (goto-char (point-max))
                                             (bibtex-beginning-of-entry)
                                             (let ((bibtex-expand-strings t))
                                               (reftex-get-bib-field "=key=" (bibtex-parse-entry t))))))
                (move-pdf-to-bib-dir (key file)
                                     (let ((target (f-join org-ref-pdf-directory (format  "%s.pdf" key))))
                                       (rename-file file target)
                                       target))
                (cleanup-on-error ()
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
                       (let ((file (move-pdf-to-bib-dir (key-of-latest-bib-entry) tmpfile)))
                         (when show-pdf
                           (find-file file))
                         (message "PDF downloaded to %s" file)))
                      (_
                       (cleanup-on-error)
                       (message "PDF download failed. See %s for details." org-funcs--wkhtmltopdf-error-buffer-name)))))
      (go))

    (run-with-timer org-funcs--pdf-download-timeout-seconds
                    nil
                    (lambda ()
                      (setq status 'timeout)))))

(provide 'org-funcs)

;;; org-funcs.el ends here
