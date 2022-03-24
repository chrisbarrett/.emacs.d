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
(require 'org-roam-review)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-roam)
  (require 'org-agenda)
  (require 'org-clock)
  (require 'org-capture))

(autoload 'org-roam-review-remove-managed-properties-in-node "org-roam-review")
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
     (ensure-empty-lines 2)
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

(defun org-funcs-simplified-title-for-url (url)
  (when-let* ((match (s-match (rx "github.com/" (group (+? nonl) (or "/issues/" "/pull/") (+ digit) eol))
                              url)))
    (cadr match)))

(defun org-funcs-guess-or-retrieve-title (url)
  (or (org-funcs-simplified-title-for-url url)
      (org-cliplink-retrieve-title-synchronously url)))

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

(defun org-funcs-roam-extract-subtree ()
  "Convert current subtree at point to a node, and extract it into a new file.

It's a re-implementation of `org-roam-extract-subtree', but
handles file titles, IDs and tags better."
  (interactive)
  (let ((dest-file (expand-file-name (format-time-string "%Y-%m-%d--%H-%M-%S.org") org-roam-directory)))
    (save-excursion
      (when (org-before-first-heading-p)
        (user-error "Already a top-level node"))

      (save-buffer)
      (org-roam-db-update-file)
      (when (file-exists-p dest-file)
        (user-error "%s exists. Aborting" dest-file))

      (let ((title (org-get-heading))
            (tags (org-get-tags))
            (id (org-id-get-create)))

        (atomic-change-group
          (save-restriction
            (org-narrow-to-subtree)
            (when (bound-and-true-p org-transclusion-mode)
              (org-transclusion-remove-all t)))

          (org-cut-subtree)
          (insert (org-link-make-string (format "id:%s" id) (org-link-display-format title)))
          (newline)
          (save-buffer)
          (with-current-buffer (find-file-noselect dest-file)
            (org-paste-subtree nil nil nil t)
            (org-roam-promote-entire-buffer)
            (org-funcs-set-title title)
            (require 'org-roam-review)
            (when-let* ((tags (-difference (-union (org-funcs--file-tags) tags)
                                           (-union org-roam-note-ignored-tags
                                                   org-roam-review-maturity-values))))
              (org-funcs--set-file-tags tags)
              (when (bound-and-true-p org-transclusion-mode)
                (org-transclusion-add-all)))
            (save-buffer)
            (run-hook-with-args 'org-roam-capture-new-node-hook)))))))

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

(defun org-funcs-roam-node-find (&optional other-window)
  "Find an org-roam node. See `org-roam-node-find'."
  (interactive "P")
  (let ((filter (lambda (node)
                  (not (seq-contains-p (org-roam-node-tags node) "dailies")))))
    (org-roam-node-find other-window nil filter)))

(defun org-funcs-new-note ()
  (let ((require-final-newline nil))
    (find-file (expand-file-name (format-time-string "%Y-%m-%d--%H-%M-%S.org")
                                 org-roam-directory))))

(defun org-funcs-new-outline-note ()
  (let ((require-final-newline nil))
    (find-file (expand-file-name (format-time-string "outlines/%Y-%m-%d--%H-%M-%S.org")
                                 org-roam-directory))))

(defun org-funcs-read-roam-node-link ()
  (let ((node (org-roam-node-read)))
    (format "[[id:%s][%s]]"
            (org-roam-node-id node)
            (org-roam-node-formatted node))))

(provide 'org-funcs)

;;; org-funcs.el ends here
