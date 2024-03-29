;;; org-funcs.el --- Extra functions to support my orgmode configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Extra functions to support org-mode config.

;;; Code:

(use-package async :ensure t)
(use-package dash :ensure t)
(use-package f :ensure t)
(use-package ht :ensure t)
(use-package org-cliplink :ensure t)

(require 'async)
(require 'dash)
(require 'dired)
(require 'f)
(require 'ht)
(require 'org-cliplink)
(require 'org-roam-rewrite)
(require 'org-roam-slipbox)
(require 'seq)
(require 'thingatpt)
(require 'timekeep)
(require 'vc-git)

(cl-eval-when (compile)
  (require 'org-roam)
  (require 'org-agenda))

(autoload 'org-clocking-p "org-clock")
(autoload 'org-project-p "org-project")
(autoload 'org-project-skip-stuck-projects "org-project")


;; Agenda utils

(defun org-funcs-agenda-dwim ()
  "Show the org agenda with appropriate tags set."
  (interactive)
  (if (org-clocking-p)
      (org-agenda nil "w")
    (org-agenda nil "p")))

(defun org-funcs--scheduled-or-deadline-p ()
  (or (org-get-scheduled-time (point))
      (org-get-deadline-time (point))))

(defun org-funcs--skip-heading-safe ()
  (or (outline-next-heading)
      (goto-char (point-max))))

(defun org-funcs--skipping-ignored-p ()
  (when-let* ((prop (-some->> (org-entry-get-with-inheritance "AGENDA_SKIP") (downcase))))
    (cond
     ((string-match-p (rx bos "ignore" eos) prop)
      t)
     ((and (string-match-p (rx bos "scheduled" eos) (downcase prop))
           (not (org-get-scheduled-time (point) t)))
      nil
      t))))

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
   ((org-funcs--skipping-ignored-p)
    nil)

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



(defun org-funcs-todo-list (tags)
  "Show the todo list for the current context.

TAGS are the tags to use when displaying the list."
  (interactive (list (if (org-clocking-p)
                         (list "-someday" "+work")
                       (list "-someday" "-work"))))
  (org-agenda prefix-arg "t")
  (org-agenda-filter-apply (cons "-ignore" tags) 'tag))


;; Capture template definition

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


;;; Commands used in capture templates

(defun org-funcs-capture-note-to-clocked-heading ()
  (unless (org-clocking-p)
    (user-error "No active clock"))
  (org-with-point-at org-clock-marker
    (org-add-note))
  "")

(defun org-funcs--last-url-kill ()
  "Return the most recent URL in the kill ring or X pasteboard."
  (--first (s-matches? (rx bos (or "http" "https" "www")) it)
           (ignore-errors
             (cons (current-kill 0 t) kill-ring))))

(defun org-funcs--strip-google-highlight-query-param (url)
  (car (split-string url (rx (? "#") ":~:"))))

(defun org-funcs-read-url (&optional prompt default)
  (let* ((default (-some->> (or default (thing-at-point-url-at-point) (org-funcs--last-url-kill))
                    (string-trim)))
         (prompt (or prompt "URL"))
         (input (read-string (concat (if default (format "%s (default %s)" prompt default) prompt) ": ")
                             nil nil default)))
    (substring-no-properties
     (if (string-match-p (rx "http" (? "s") "://") input)
         (org-funcs--strip-google-highlight-query-param input)
       (org-funcs-read-url prompt default)))))

(defun org-funcs--simplify-url (regexp url)
  (-some->> (s-match regexp url)
    (cdr) ;; 0 is the whole string
    (seq-map (lambda (it) (s-replace "+" " " it)))
    (s-join "/…/")))


(defun org-funcs-simplified-title-for-url (url)

  (cl-labels ((extract (host regexp &optional exclude-host-p)
                       (when-let* ((simplified (org-funcs--simplify-url regexp url)))
                         (if exclude-host-p
                             simplified
                           (format "%s (%s)" simplified host)))))
    (let ((query '(? "?" (* nonl))))
      (or
       (extract "Confluence"
                (rx ".atlassian.net/wiki/spaces/" (+? nonl) "/pages/" (+? nonl) "/" (group (+ nonl))))
       (extract "Jira"
                (rx-to-string `(and ".atlassian.net/browse/" (group (+? nonl)) ,query eol)))

       (extract "GitHub"
                (rx-to-string `(and bol "https://github.com/"
                                    (group (+? nonl) (or "/issues/" "/pull/") (+ digit))
                                    ,query eol)))

       ;; (org-funcs-simplified-title-for-url "https://linear.app/COMPANY/issue/KEY-0000/the-quick-brown-fox")
       (-some->> (extract "Linear"
                          (rx-to-string `(and bol "https://linear.app/"
                                              (+ alnum) "/issue/" (+? nonl) "/" (group (+? nonl)) ,query eol))
                          'no-host)
         (s-replace "-" " "))

       ;; (org-funcs-simplified-title-for-url "https://trello.com/c/00000000/1000-the-quick-brown-fox")
       (-some->> (extract "Trello"
                          (rx-to-string `(and bol "https://trello.com/c/"
                                              (+ alnum) "/" (+ digit) "-" (group (+? nonl)) ,query eol)))
         (s-replace "-" " "))

       ;; (org-funcs-simplified-title-for-url "https://app.shortcut.com/companydomain/story/10000/the-quick-brown-fox")
       (-some->> (extract "Shortcut"
                          (rx-to-string `(and bol "https://app.shortcut.com/"
                                              (+? any) "/" (+? any) "/" (+ digit) "/" (group (+? nonl)) ,query eol)))
         (s-replace "-" " "))

       ;; (org-funcs-simplified-title-for-url "https://github.com/org/repo/blob/master/path/file.md")
       (extract "GitHub"
                (rx bol "https://github.com/"
                    (group (+? nonl) "/" (+ nonl))
                    "/blob/" (+? nonl) "/"
                    (group (+ nonl))))


       ;; (org-funcs-simplified-title-for-url "https://github.com/org/repo")
       (extract "GitHub" (rx bol "https://github.com/" (group (+ nonl))))

       (when (string-match-p (rx bol "https://" (+? any) ".slack.com/") url)
         "Slack link")))))

(defun org-funcs--postprocess-retrieved-title (url title)
  (string-trim (cond
                ((string-match-p (rx "investopedia.com") url)
                 (concat title " (Investopedia)"))
                ((string-suffix-p "| Microsoft Docs" title)
                 (string-remove-suffix "| Microsoft Docs" title))
                ((string-prefix-p "https://developer.apple.com/library/archive/documentation/" url)
                 (concat title " (Apple Developer Archive)"))
                (t
                 title))))

(defun org-funcs-guess-or-retrieve-title (url)
  (let ((title
         (or (org-funcs-simplified-title-for-url url)
             (org-funcs--postprocess-retrieved-title url
                                                     (let ((org-cliplink-max-length 1024))
                                                       (org-cliplink-retrieve-title-synchronously url))))))
    (org-cliplink-elide-string title org-cliplink-max-length)))

(defun org-funcs-insert-url-as-link (url)
  "Insert an orgmode link at point for URL."
  (interactive (list (org-funcs-read-url)))
  (save-match-data
    (let ((title (or (org-funcs-guess-or-retrieve-title url)
                     (read-string "Title: "))))
      (unless (thing-at-point-looking-at (rx bol (* space)))
        (just-one-space))
      (insert (org-link-make-string url title))
      (just-one-space))))

(defun org-funcs-roam-node-find (&optional other-window)
  "Find an org-roam node. See `org-roam-node-find'.

With optional prefix arg OTHER-WINDOW, visit the node in another
window."
  (interactive "P")
  (org-roam-node-find other-window
                      nil
                      (lambda (node)
                        (let* ((tags (org-roam-node-tags node))
                               (disallowed (flatten-list (list '("dailies" "litnotes")
                                                               (when (and timekeep-mode (org-clocking-p))
                                                                 "private")))))
                          (null (seq-intersection tags disallowed))))))

(defun org-funcs-read-roam-node-link ()
  (let ((node (org-roam-node-read)))
    (org-link-make-string
     (concat "id:"
             (org-roam-node-id node))
     (org-roam-node-formatted node))))

(defun org-funcs-follow-link-other-window ()
  "Force the link at point to open in another window."
  (interactive)
  (let ((org-link-frame-setup (cons '(file .
                                           (lambda ()
                                             (when-let* ((buf (find-file-noselect filename)))
                                               (display-buffer-in-direction buf '((direction . rightmost))))))
                                    org-link-frame-setup)))
    (org-open-at-point)))


;; BibTeX management with Citar

(defconst org-funcs-key-sequence-for-lit-note-capture-template "rn"
  "The key sequence for the literature note capture template.")

(defvar org-funcs--cite-key-for-capture nil
  "Side-channel variable used to inject citation read from prompt.

It should only ever be dynamically bound.")

(autoload 'citar-select-ref "citar")

(defun org-funcs-citation-from-capture-or-read ()
  "Get a citar reference for use in an org capture template.

If capture was triggered by trying to navigate to non-existent a
notes file, return the reference that was originally selected by
the user.

Otherwise, prompt the user for a reference."
  (or org-funcs--cite-key-for-capture
      (citar-select-ref)))

(defun org-funcs-go-to-litnote-for-key (key &optional attrs)
  "Function for use as `citar-open-note-function', which see.

KEY is a cite key.

ATTRS are additional attributes for the citation passed by
citar."
  (if-let* ((node (org-roam-node-from-ref (concat "@" key))))
      (org-roam-node-visit node)
    (let ((org-funcs--cite-key-for-capture (cons key attrs)))
      (org-capture nil org-funcs-key-sequence-for-lit-note-capture-template))))

(defun org-funcs-clean-bibtex-string (s)
  "Remove quoting brackets and superfluous whitespace from string S."
  (when s
    (->> (replace-regexp-in-string "[\"{}]+" "" s)
         (replace-regexp-in-string "[\n\t ]+" " ")
         (string-replace "\\" ""))))

(defun org-funcs-add-roam-cite-ref (key)
  "Add KEY as a ROAM_REFS cite entry for the current node."
  (interactive (list (citar-select-ref)))
  (org-roam-ref-add (format "[cite:@%s]" key)))

(provide 'org-funcs)

;;; org-funcs.el ends here
