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
(require 'org-roam-review)
(require 'seq)
(require 'thingatpt)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-roam)
  (require 'org-transclusion)
  (require 'org-agenda)
  (require 'org-clock)
  (require 'org-capture))

(autoload 'timekeep-work-tag "timekeep")
(autoload 'org-cliplink-retrieve-title-synchronously "org-cliplink")
(autoload 'org-project-p "org-project")
(autoload 'org-project-skip-stuck-projects "org-project")
(autoload 'org-transclusion-remove-all "org-transclusion")


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

(defun org-funcs-inline-note (src-node dest-node)
  "Inline the contents of one org-roam note into another, removing the original."
  (interactive
   (let* ((src (org-roam-node-read (-some->> (org-roam-node-at-point) (org-roam-node-title)) nil nil t "Source: "))
          (dest (org-roam-node-read nil (lambda (node)
                                          (and
                                           (not (equal (org-roam-node-id node) (org-roam-node-id src)))
                                           (zerop (org-roam-node-level node))
                                           (not (seq-contains-p (org-roam-node-tags node) "dailies"))))
                                    nil t "Destination: ")))
     (list src dest)))

  (let* ((org-inhibit-startup t)
         (src-buffer (find-file-noselect (org-roam-node-file src-node)))
         (content
          (with-current-buffer src-buffer
            (org-with-wide-buffer
             (org-transclusion-remove-all)
             (goto-char (point-min))
             (org-roam-end-of-meta-data t)
             (buffer-substring (point) (point-max))))))
    (find-file (org-roam-node-file dest-node))
    (org-with-wide-buffer
     (org-transclusion-remove-all)
     (goto-char (point-max))
     (delete-blank-lines)
     (insert "\n\n")
     (insert (format "* %s\n" (org-roam-node-title src-node)))
     (org-set-property "ID" (org-roam-node-id src-node))
     (save-restriction
       (narrow-to-region (point) (point-max))
       (insert content)
       (org-map-entries 'org-do-demote)
       (goto-char (point-min))
       (while (search-forward-regexp (rx bol "#+transclude:") nil t)
         (org-transclusion-add)
         (org-transclusion-promote-subtree))))
    (delete-file (org-roam-node-file src-node))
    (save-buffer)
    (org-transclusion-add-all)
    (when (buffer-live-p src-buffer)
      (kill-buffer src-buffer)))

  (org-roam-node-visit dest-node)
  (message "Inlined note successfully"))

(defun org-funcs--rewrite-backlinks (backlinks new-id new-title)
  (let ((replacement (org-link-make-string (concat "id:" new-id) new-title))
        (backlinks-by-file
         (seq-group-by (-compose #'org-roam-node-file #'org-roam-backlink-source-node)
                       backlinks)))
    (pcase-dolist (`(,file . ,backlinks) backlinks-by-file)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (backlink (seq-sort-by #'org-roam-backlink-point #'> backlinks))
          (goto-char (org-roam-backlink-point backlink))
          (save-match-data
            (looking-at org-link-any-re)
            (replace-match replacement t t)))
        (write-region (point-min) (point-max) file)))))

(defun org-funcs--update-node-title (node new-title)
  (org-id-goto (org-roam-node-id node))
  (cond ((equal 0 (org-roam-node-level node))
         (org-funcs-set-title new-title))
        ((looking-at org-complex-heading-regexp)
         (replace-match new-title t t nil 4)))
  (save-buffer))

(defun org-funcs-rename-note (node new-title)
  "Change the title of a note and update links to match.

NODE is the node to update.

NEW-TITLE is the new title to use. All backlinks will have their
descriptions updated to this value."
  (interactive (let* ((node (org-roam-node-read (-some->> (org-roam-node-at-point) (org-roam-node-title))
                                                nil nil t "Rename: ")))
                 (list node (read-string "New title: " (org-roam-node-title node)))))
  (org-roam-node-visit node)
  (org-save-all-org-buffers)
  (let ((backlinks (org-roam-backlinks-get node)))
    (cond
     ((null backlinks)
      (org-funcs--update-node-title node new-title)
      (message "No backlinks found."))
     ((y-or-n-p (format "Rewriting %s link(s) from \"%s\" -> \"%s\". Continue? "
                        (length backlinks) (org-roam-node-title node) new-title))
      (org-funcs--update-node-title node new-title)
      (org-funcs--rewrite-backlinks backlinks (org-roam-node-id node) new-title)
      (message "Rewrote %s links to note." (length backlinks)))
     (t
      (user-error "Rewrite aborted")))))

(defun org-funcs--delete-org-roam-node (node)
  (when-let* ((buf (find-buffer-visiting (org-roam-node-file node))))
    (kill-buffer buf))
  (delete-file (org-roam-node-file node)))

(defun org-funcs-rewrite-note-links (from to link-desc)
  "Redirect links from one node to a replacement node.

FROM is the node which will be unlinked.

TO is the node to change those references to point to.

LINK-DESC is the description to use for the updated links."
  (interactive (let* ((from (org-roam-node-read nil nil nil t "Remove: "))
                      (to (org-roam-node-read nil (lambda (it) (not (equal from it))) nil t "Rewrite to: ")))
                 (list from to (read-string "Link description: " (org-roam-node-title to)))))
  (org-save-all-org-buffers)
  (let ((backlinks (org-roam-backlinks-get from)))
    (cond
     ((null backlinks)
      (when (y-or-n-p "No links found. Delete note? ")
        (org-funcs--delete-org-roam-node from)))
     ((y-or-n-p (format "Rewriting %s link(s) from \"%s\" -> \"%s\". Continue? "
                        (length backlinks) (org-roam-node-title from) link-desc))
      (org-funcs--rewrite-backlinks backlinks (org-roam-node-id to) link-desc)
      (when (y-or-n-p "Rewrite completed. Delete note? ")
        (org-funcs--delete-org-roam-node from)))
     (t
      (user-error "Rewrite aborted")))))



(defun org-funcs-set-title (text)
  (org-with-wide-buffer
   (goto-char (point-min))
   (save-match-data
     (search-forward-regexp (rx bol "#+title:" (* space) (group (+ any)) eol))
     (replace-match text t nil nil 1))))

(defun org-funcs-title ()
  (org-with-wide-buffer
   (goto-char (point-min))
   (save-match-data
     (search-forward-regexp (rx bol "#+title:" (* space) (group (+ any)) eol))
     (match-string 1 ))))


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

(defun org-funcs-simplified-title-for-url (url)
  (let ((query-params '(? "?" (* nonl))))
    (or
     (cadr
      (or (s-match (rx-to-string `(and "github.com/" (group (+? nonl) (or "/issues/" "/pull/") (+ digit)) ,query-params eol)) url)
          (s-match (rx-to-string `(and "atlassian.net/browse/" (group (+? nonl)) ,query-params eol)) url)))
     (cond
      ((string-match-p (rx bol "https://" (+? any) ".slack.com/") url)
       "Slack link")))))

(defun org-funcs--postprocess-retrieved-title (url title)
  (cond
   ((string-match-p (rx "investopedia.com") url)
    (concat title " (Investopedia)"))
   (t
    title)))

(defun org-funcs-guess-or-retrieve-title (url)
  (or (org-funcs-simplified-title-for-url url)
      (org-funcs--postprocess-retrieved-title url
                                              (org-cliplink-retrieve-title-synchronously url))))

(defun org-funcs-insert-url-as-link (url)
  "Insert an orgmode link at point for URL."
  (interactive (list (org-funcs-read-url)))
  (save-match-data
    (let* ((title (or (org-funcs-guess-or-retrieve-title url)
                      (read-string "Title: ")))
           (escaped-title (s-replace-all '(("[" . "(")
                                           ("]" . ")"))
                                         title)))
      (unless (thing-at-point-looking-at (rx bol (* space)))
        (just-one-space))
      (insert (format "[[%s][%s]]" url escaped-title))
      (just-one-space))))

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

(defvar org-funcs-extra-tags-excluded-for-extraction '("moc"))

(defun org-funcs-roam-extract-subtree ()
  "Convert current subtree at point to a node, and extract it into a new file.

It's a re-implementation of `org-roam-extract-subtree', but
handles file titles, IDs and tags better."
  (interactive)
  (let ((dest-file (f-join org-roam-directory "notes" (format-time-string "%Y-%m-%d--%H-%M-%S.org"))))
    (save-excursion
      (when (org-before-first-heading-p)
        (user-error "Already a top-level node"))

      (save-buffer)
      (org-roam-db-update-file)
      (when (file-exists-p dest-file)
        (user-error "%s exists. Aborting" dest-file))

      (atomic-change-group
        (let ((title (org-get-heading))
              (tags (org-get-tags))
              (id (org-id-get-create)))

          (save-restriction
            (org-narrow-to-subtree)
            (when (bound-and-true-p org-transclusion-mode)
              (org-transclusion-remove-all t)))

          (org-cut-subtree)
          (insert (org-link-make-string (format "id:%s" id) (org-link-display-format title)))
          (newline)
          (save-buffer)
          (f-touch dest-file)
          (with-current-buffer (find-file-noselect dest-file)
            (condition-case _
                (progn
                  (org-paste-subtree nil nil nil t)
                  (org-roam-promote-entire-buffer)
                  (org-funcs-set-title title)
                  (require 'org-roam-review)
                  (let ((tags-to-remove (append org-roam-review-ignored-tags
                                                org-roam-review-maturity-values
                                                org-funcs-extra-tags-excluded-for-extraction)))
                    (when-let* ((tags (-difference (-union (org-funcs--file-tags) tags)
                                                   tags-to-remove)))
                      (org-funcs--set-file-tags tags)
                      (when (bound-and-true-p org-transclusion-mode)
                        (org-transclusion-add-all))))
                  (save-buffer))
              (error
               (f-delete dest-file)))

            (run-hook-with-args 'org-roam-capture-new-node-hook)))))))

(defun org-funcs-roam-node-find (&optional other-window)
  "Find an org-roam node. See `org-roam-node-find'."
  (interactive "P")
  (let ((filter (lambda (node)
                  (let ((tags (org-roam-node-tags node)))
                    (or (null (seq-intersection tags '("outline" "dailies")))
                        ;; Exclude subnotes in outlines
                        (when (seq-contains-p tags "outline")
                          (= 0 (org-roam-node-level node))))))))
    (org-roam-node-find other-window nil filter)))

(defun org-funcs-read-roam-node-link ()
  (let ((node (org-roam-node-read)))
    (format "[[id:%s][%s]]"
            (org-roam-node-id node)
            (org-roam-node-formatted node))))

(defun org-funcs-roam-node-to-link-string (node)
  (org-link-make-string (concat "id:" (org-roam-node-id node))
                        (org-roam-node-title node)))



(defconst org-funcs-key-sequence-for-outline-capture-template "ro"
  "The key sequence for the outline note capture template.")

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

(defun org-funcs-go-to-outline-for-key (key &optional attrs)
  "See `citar-open-note-function'."
  (if-let* ((node (org-roam-node-from-ref (concat "@" key))))
      (org-roam-node-visit node)
    (let ((org-funcs--cite-key-for-capture (cons key attrs)))
      (org-capture nil org-funcs-key-sequence-for-outline-capture-template))))



(defun org-funcs-clean-bibtex-string (s)
  "Remove quoting brackets and superfluous whitespace from string S."
  (string-replace "\\&" "&"
                  (replace-regexp-in-string "[\n\t ]+" " "
                                            (replace-regexp-in-string "[\"{}]+" "" s))))

(defun org-funcs-add-roam-cite-ref (key)
  "Add KEY as a ROAM_REFS cite entry for the current node."
  (interactive (list (citar-select-ref)))
  (org-roam-ref-add (format "[cite:@%s]" key)))

(provide 'org-funcs)

;;; org-funcs.el ends here
