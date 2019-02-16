;;; config-org.el --- Orgmode configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-major-mode-hydra)
(require 'dash)
(require 'f)
(require 'general)
(require 'paths)
(require 's)
(require 'straight)
(require 'subr-x)

(autoload 'org-todo "org")
(autoload 'jira-utils-read-issue-url-for-org-header "jira-utils")



(with-no-warnings
  (cb-major-mode-hydra-define org-mode
    "Tree"
    (("a" org-archive-subtree "archive")
     ("r" org-refile "refile")
     ("t" org-show-todo-tree "todo tree"))

    "Misc"
    (("e" org-babel/body "babel commands")
     ("p" org-present "present"))))


;; forward-definitions to silence byte-compiler.

(defvar org-done-keywords)
(defvar org-directory "~/org")



(general-setq org-directory "~/org")

(defvar config-org-work-file (f-join org-directory "work_pushpay.org"))

(general-setq
 org-default-notes-file (f-join org-directory "notes.org")
 org-agenda-diary-file (f-join org-directory "diary.org")
 org-agenda-files (f-files org-directory (lambda (f) (f-ext? f "org"))))


;; general settings

(general-setq
 org-imenu-depth 4
 org-M-RET-may-split-line nil
 org-blank-before-new-entry '((heading . always) (plain-list-item . nil))
 org-catch-invisible-edits 'smart
 org-checkbox-hierarchical-statistics t
 org-confirm-elisp-link-function nil
 org-cycle-separator-lines 1
 org-enforce-todo-dependencies t
 org-footnote-auto-adjust t
 org-hide-emphasis-markers t
 org-hierarchical-todo-statistics nil
 org-indirect-buffer-display 'current-window
 org-insert-heading-respect-content t
 org-log-done 'time
 org-log-into-drawer t
 org-log-repeat nil
 org-outline-path-complete-in-steps nil
 org-pretty-entities t
 org-refile-allow-creating-parent-nodes 'confirm
 org-refile-target-verify-function (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords)))
 org-refile-use-outline-path t
 org-return-follows-link t
 org-reverse-note-order nil
 org-startup-indented t
 org-startup-with-inline-images t

 org-todo-keywords '((type "TODO(t)" "MAYBE(m)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
                     (type "SOMEDAY(s)" "|"))

 org-src-fontify-natively t
 org-src-window-setup 'current-window

 ;; org-babel

 org-confirm-babel-evaluate nil

 ;; org-export

 org-export-backends '(ascii html latex odt gfm koma-letter custom-confluence)
 org-html-html5-fancy t
 org-html-postamble nil
 org-export-exclude-tags '("noexport" "no_export" "crypt")
 org-export-coding-system 'utf-8

 ;; org-habit

 org-habit-graph-column 68
 org-habit-preceding-days 7
 org-habit-following-days 5

 org-tags-exclude-from-inheritance '("crypt" "project")
 org-crypt-disable-auto-save 'encypt

 org-refile-targets
 '((nil . (:maxlevel . 3))
   (org-default-notes-file :maxlevel . 3)
   (org-directory :maxlevel . 3))

 org-attach-directory (f-join org-directory "data")
 org-archive-default-command #'config-org--archive-done-tasks

 ;; appt

 appt-message-warning-time 60
 appt-display-interval 5

 ;; clocking

 org-clock-history-length 20
 org-clock-in-resume t
 org-clock-out-remove-zero-time-clocks t
 org-clock-persist t
 org-clock-persist-query-resume nil
 org-clock-report-include-clocking-task t
 )


;; org-agenda settings

(general-setq
 ;; Match projects that do not have any todos.
 org-stuck-projects '("+project-ignore-maybe-done" ("TODO") nil)
 org-agenda-auto-exclude-function #'config-org--exclude-tasks-on-hold
 org-agenda-hide-tags-regexp (rx (or "noexport" "someday" "project"))
 org-agenda-include-diary nil
 org-agenda-insert-diary-extract-time t
 org-agenda-search-view-always-boolean t
 org-agenda-show-all-dates nil
 org-agenda-show-inherited-tags nil
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-deadline-prewarning-if-scheduled t
 org-agenda-skip-scheduled-if-done t
 org-agenda-span 'week
 org-agenda-start-on-weekday nil
 org-agenda-window-setup 'only-window
 org-agenda-dim-blocked-tasks 'invisible
 org-agenda-sorting-strategy '((agenda time-up priority-down category-keep)
                               (todo priority-down category-keep scheduled-up)
                               (tags priority-down category-keep)
                               (search category-keep))
 org-agenda-clockreport-parameter-plist (list :compact t :maxlevel 5 :fileskip0 t :step 'week)
 org-agenda-inhibit-startup t
 org-agenda-tags-column -100
 org-agenda-text-search-extra-files '(agenda-archives)
 org-agenda-use-time-grid nil
 org-agenda-category-icon-alist `(("Emacs"
                                   ,(list (all-the-icons-fileicon "emacs" :height 0.8 :v-adjust 0.05))
                                   nil nil
                                   :ascent center)
                                  ("projects?"
                                   ,(list (all-the-icons-octicon "repo" :v-adjust 0.05))
                                   nil
                                   nil
                                   :ascent center)
                                  ("goals?"
                                   ,(list (all-the-icons-octicon "checklist" :v-adjust 0.05))
                                   nil
                                   nil
                                   :ascent center)
                                  ("car"
                                   ,(list (all-the-icons-material "directions_car"))
                                   nil
                                   nil
                                   :ascent center)
                                  ("gtd"
                                   ,(list (all-the-icons-faicon "check-square-o" :v-adjust 0.05))
                                   nil
                                   nil
                                   :ascent center)
                                  (,(rx (or "Holiday" "Birthday"))
                                   ,(list (all-the-icons-faicon "calendar-o" :v-adjust 0.05))
                                   nil
                                   nil
                                   :ascent center)
                                  (,(rx (or "flat" "bill" "income"))
                                   ,(list (all-the-icons-material "monetization_on"))
                                   nil
                                   nil
                                   :ascent center)
                                  ("notes"
                                   ,(list (all-the-icons-faicon "tasks" :height 0.9 :v-adjust 0.05))
                                   nil
                                   nil
                                   :ascent center)))

(general-setq org-agenda-custom-commands
              '(("A" "Agenda and next actions"
                 ((tags-todo "-study-someday-media-gtd/TODO"
                             ((org-agenda-overriding-header "Next Actions")
                              ;; Take the first item from each todo list. Also
                              ;; exclude items with scheduled/deadline times, since
                              ;; they show up in the calendar views.
                              (org-agenda-skip-function (lambda ()
                                                          (or (config-org--agenda-skip-if-has-timestamp)
                                                              (config-org--agenda-skip-all-siblings-but-first))))))
                  (agenda "")
                  (todo "WAITING"
                        ((org-agenda-overriding-header "Waiting")))
                  (stuck "")
                  (tags-todo "media|study/TODO"
                             ((org-agenda-overriding-header "Media & Study"))))
                 ((org-agenda-tag-filter-preset '("-ignore"))
                  (org-agenda-files (list org-default-notes-file org-agenda-diary-file))
                  (org-agenda-archives-mode nil)
                  (org-agenda-ignore-drawer-properties '(effort appt))))

                ("w" "Work actions"
                 ((tags-todo "-study-someday-media-gtd/TODO"
                             ((org-agenda-overriding-header "Next Actions")
                              (org-agenda-skip-function (lambda ()
                                                          (or (config-org--agenda-skip-if-has-timestamp)
                                                              (config-org--agenda-skip-all-siblings-but-first))))))
                  (todo "WAITING"
                        ((org-agenda-overriding-header "Waiting")))
                  (stuck "")
                  (agenda "")
                  (tags "+standup"
                        ((org-agenda-overriding-header "Standup"))))
                 ((org-agenda-tag-filter-preset '("-ignore"))
                  (org-agenda-use-tag-inheritance nil)
                  (org-agenda-files (list config-org-work-file org-agenda-diary-file))
                  (org-agenda-archives-mode nil)
                  (org-agenda-ignore-drawer-properties '(effort appt))))

                ("r" "Weekly Review"
                 ((agenda ""
                          ((org-agenda-overriding-header "Review Previous Week")
                           (org-agenda-ndays 7)
                           (org-agenda-start-day "-7d")))
                  (agenda ""
                          ((org-agenda-overriding-header "Review Upcoming Events")
                           (org-agenda-ndays 14)))
                  (stuck ""
                         ((org-agenda-overriding-header "Review Stuck Projects")))
                  (todo "WAITING"
                        ((org-agenda-overriding-header "Review Tasks on Hold")))

                  (tags-todo "-someday-media/TODO"
                             ((org-agenda-overriding-header "Next Actions")
                              (org-agenda-skip-function (lambda ()
                                                          (or (config-org--agenda-skip-if-has-timestamp)
                                                              (config-org--agenda-skip-all-siblings-but-first))))))
                  (tags-todo "+goals+3_months+project/TODO"
                             ((org-agenda-overriding-header "Review 3 Month Goals")
                              (org-agenda-skip-function (lambda ()
                                                          (or (config-org--agenda-skip-if-has-timestamp)
                                                              (config-org--agenda-skip-all-siblings-but-first))))))
                  (tags-todo "+goals+1_year+project/TODO"
                             ((org-agenda-overriding-header "Review 1 Year Goals")
                              (org-agenda-skip-function (lambda ()
                                                          (or (config-org--agenda-skip-if-has-timestamp)
                                                              (config-org--agenda-skip-all-siblings-but-first))))))
                  (tags-todo "+goals+3_years+project/MAYBE|SOMEDAY|TODO"
                             ((org-agenda-overriding-header "Review 3 Year Goals")
                              (org-agenda-skip-function (lambda ()
                                                          (or (config-org--agenda-skip-if-has-timestamp)
                                                              (config-org--agenda-skip-all-siblings-but-first))))))
                  (tags-todo "someday-skill/MAYBE|TODO"
                             ((org-agenda-overriding-header "Decide whether to promote any SOMEDAY items to TODOs")))
                  (tags-todo "someday&skill"
                             ((org-agenda-overriding-header "Decide whether to promote any learning tasks to TODOs"))))
                 ((org-agenda-tag-filter-preset
                   '("-gtd" "-ignore"))
                  (org-agenda-include-inactive-timestamps t)
                  (org-agenda-files (list org-default-notes-file config-org-work-file org-agenda-diary-file))
                  (org-agenda-archives-mode nil)))))

(defun config-org-agenda-open-nth-view (n)
  (when-let ((choice (car (nth n org-agenda-custom-commands))))
    (org-agenda nil choice)))

(defun config-org-agenda-nth-name (n)
  (or (cadr (nth n org-agenda-custom-commands))
      ""))

(pretty-hydra-define config-org-agenda
  (:hint nil :foreign-keys run :quit-key "q"
   :body-pre (config-org-agenda-open-nth-view 0)
   :post (org-agenda-quit))
  ("Agenda Views"
   (("1" (config-org-agenda-open-nth-view 0) (config-org-agenda-nth-name 0))
    ("2" (config-org-agenda-open-nth-view 1) (config-org-agenda-nth-name 1))
    ("3" (config-org-agenda-open-nth-view 2) (config-org-agenda-nth-name 2)))))


;; org-capture templates

(autoload 'cb-org-team-log-heading "cb-org-team")

(general-setq org-capture-templates
              (cl-labels ((entry
                           (key label form template
                                &key
                                immediate-finish
                                jump-to-captured
                                (type 'entry)
                                (prepend t)
                                (clock-keep t)
                                (empty-lines 1))
                           (list key label type form template
                                 :clock-keep clock-keep
                                 :empty-lines empty-lines
                                 :prepend prepend
                                 :immediate-finish immediate-finish
                                 jump-to-captured :jump-to-captured)))
                (list
                 (entry
                  "t" "Todo" '(file org-default-notes-file) "* TODO %?")
                 (entry
                  "s" "Someday" '(file+olp org-default-notes-file "Someday")
                  "* SOMEDAY %?")
                 (entry
                  "l" "Link" '(file+olp org-default-notes-file "Links") '(function cb-org-capture-url-read-url)
                  :immediate-finish t)
                 (entry
                  "r" "Reading" '(file+olp org-default-notes-file "Media" "Reading")
                  "* MAYBE Read %i%?")
                 (entry
                  "R" "Recipe" '(file+olp org-default-notes-file "Recipes") '(function org-chef-get-recipe-from-url)
                  :jump-to-captured t
                  :immediate-finish t)
                 (entry
                  "m" "Listening" '(file+olp org-default-notes-file "Media" "Listening")
                  "* MAYBE Listen to %i%?")
                 (entry
                  "v" "Viewing" '(file+olp org-default-notes-file "Media" "Viewing")
                  "* MAYBE Watch %i%?")
                 (entry
                  "e" "Email task" '(file org-default-notes-file)
                  "* TODO %?\n%a")

                 '("w" "Work")
                 (entry
                  "wa" "Team member activity"
                  '(file+function config-org-work-file cb-org-team-log-heading)
                  "- %?"
                  :type 'item
                  :jump-to-captured t)
                 (entry
                  "wt" "Todo"
                  `(file ,config-org-work-file) "* TODO %?")
                 (entry
                  "wl" "Link"
                  `(file+olp config-org-work-file "Links")
                  '(function cb-org-capture-url-read-url)
                  :immediate-finish t)
                 (entry
                  "wj" "Jira issue reference"
                  `(file config-org-work-file)
                  '(function jira-utils-read-issue-url-for-org-header)
                  :jump-to-captured t
                  :immediate-finish t
                  :type 'item)
                 (entry
                  "we" "Email task"
                  `(file config-org-work-file) "* TODO %?\n%a"))))



;; HACK: Dummy definitions to fix org html export incompatability.
(defvar font-lock-beg nil)
(defvar font-lock-end nil)

(use-package gnuplot :straight t :defer t)
(use-package ob-restclient :straight t :defer t)
(use-package ox-gfm :straight t :after org)

;; Let's not fuck this one up.
(eval-and-compile
  (defconst cb-org-load-path (expand-file-name "straight/build/org/" user-emacs-directory)))

(use-package org
  :straight org-plus-contrib
  :defer t
  :load-path cb-org-load-path
  :general
  (:states 'normal :keymaps 'org-mode-map "RET" #'org-return)
  (:keymaps 'org-mode-map
   "C-c C-." #'org-time-stamp-inactive
   "M-p" #'org-metaup
   "M-n" #'org-metadown
   "C-c c" #'org-columns)

  :preface
  (progn
    (autoload 'org-entry-get "org")
    (autoload 'org-get-scheduled-time "org")
    (autoload 'org-get-todo-state "org")
    (autoload 'org-heading-components "org")
    (autoload 'org-todo "org")
    (autoload 'org-up-heading-safe "org")
    (autoload 'outline-forward-same-level "outline")
    (autoload 's-matches? "s")

    ;; KLUDGE: Pre-declare dynamic variables used by orgmode.
    (defvar org-state)
    (defvar org-log-states)
    (defvar org-log-done)

    (defun config-org--exit-minibuffer (&rest _)
      "Exit minibuffer before adding notes."
      (when (minibufferp (window-buffer (selected-window)))
        (other-window 1)))

    (defun config-org--toggle-heading-goto-eol (&rest _)
      "Prevent point from moving to BOL when toggling headings."
      (when (s-matches? (rx bol (+ "*") (* space) eol)
                        (buffer-substring (line-beginning-position) (line-end-position)))
        (goto-char (line-end-position))))

    (defun config-org--mark-next-parent-tasks-todo ()
      "Visit each parent task and change state to TODO."
      (when-let* ((mystate (or (bound-and-true-p org-state)
                               (nth 2 (org-heading-components)))))
        (save-excursion
          (while (org-up-heading-safe)
            (when (-contains? '("WAITING" "MAYBE")
                              (nth 2 (org-heading-components)))
              (org-todo "TODO"))))))

    (defun config-org--set-bidi-env ()
      (setq bidi-paragraph-direction nil))

    (defun config-org--add-local-hooks ()
      "Set buffer-local hooks for orgmode."
      (add-hook 'org-after-todo-state-change-hook #'config-org--mark-next-parent-tasks-todo nil t)
      (add-hook 'org-clock-in-hook #'config-org--mark-next-parent-tasks-todo nil t))

    (defun config-org--children-done-parent-done (_n-done n-todo)
      "Mark the parent task as done when all children are completed."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (zerop n-todo) "DONE" "TODO")))))

  :commands
  (org-refile)

  :init
  (progn
    (add-hook 'org-mode-hook #'config-org--add-local-hooks)
    (add-hook 'org-mode-hook #'config-org--set-bidi-env)
    (add-hook 'org-after-todo-statistics-hook #'config-org--children-done-parent-done)

    (dolist (dir (ignore-errors (f-directories "~/org/lisp/")))
      (add-to-list 'load-path (f-slash dir))))

  :config
  (progn
    (with-eval-after-load 'evil
      (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle))

    (general-setq org-babel-load-languages
                  '((emacs-lisp . t)
                    (restclient . t)
                    (js . t)
                    (gnuplot . t)
                    (python . t)
                    (shell . t)
                    (sql . t)))

    (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file-other-window)

    (add-to-list 'org-latex-default-packages-alist
                 '("colorlinks=true" "hyperref" nil))

    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    (advice-add 'org-add-log-note :before #'config-org--exit-minibuffer)
    (advice-add 'org-toggle-heading :after #'config-org--toggle-heading-goto-eol)))

(use-package cb-org-capture-url :after org)
(use-package cb-org-gdrive :hook (org-mode . cb-org-gdrive-init))
(use-package cb-org-pgp-decrpyt :hook (org-mode . cb-org-pgp-decrpyt-init))
(use-package ob-gnuplot :after org)
(use-package ob-shell :after org)
(use-package ob-sql :after org)
(use-package org-habit :after org-agenda)
(use-package org-hydras :commands (org-babel/body))

(use-package org-agenda
  :after org
  :general
  (:keymaps 'org-agenda-mode-map :states 'motion
   "t" #'org-agenda-todo
   "J" #'org-agenda-goto-date
   "j" #'org-agenda-next-line
   "k" #'org-agenda-previous-line
   "f" #'org-agenda-later
   "b" #'org-agenda-earlier
   "M-j" #'org-agenda-next-item
   "M-k" #'org-agenda-previous-item
   "M-h" #'org-agenda-earlier
   "M-l" #'org-agenda-later
   "gd" #'org-agenda-toggle-time-grid
   "gr" #'org-agenda-redo
   "M-RET" #'org-agenda-show-and-scroll-up
   "C-f" #'evil-scroll-page-down
   "C-b" #'evil-scroll-page-up)
  :preface
  (progn
    (autoload 'org-get-deadline-time "org")
    (autoload 'org-goto-sibling "org")
    (autoload 'outline-next-heading "outline")
    (autoload 'page-break-lines--update-display-tables "page-break-lines")

    (defun config-org--draw-separator (&rest _)
      (page-break-lines--update-display-tables))

    (defun config-org--exclude-tasks-on-hold (tag)
      (and (equal tag "hold") (concat "-" tag)))

    (defun config-org--agenda-skip-if-has-timestamp ()
      "Skip the item if it has a scheduled or deadline timestamp."
      (when (or (org-get-scheduled-time (point))
                (org-get-deadline-time (point)))
        (or (outline-next-heading)
            (goto-char (point-max)))))

    (defun config-org--current-headline-is-todo ()
      (string= "TODO" (org-get-todo-state)))

    (defun config-org--agenda-skip-all-siblings-but-first ()
      "Skip all but the first non-done entry."
      (let (should-skip-entry)
        (unless (config-org--current-headline-is-todo)
          (setq should-skip-entry t))
        (save-excursion
          (while (and (not should-skip-entry) (org-goto-sibling t))
            (when (config-org--current-headline-is-todo)
              (setq should-skip-entry t))))
        (when should-skip-entry
          (or (outline-next-heading)
              (goto-char (point-max)))))))

  :config
  (progn
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

    ;; Ensure the separator line is rendered whenever the org agenda view
    ;; changes. This is needed for page-break-lines to render the separator
    ;; correctly.
    (setq org-agenda-block-separator "")
    (advice-add 'org-agenda :after #'config-org--draw-separator)
    (advice-add 'org-agenda-redo :after #'config-org--draw-separator)))

(use-package org-table
  :after org
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Edit Formulas*" eos)
                 (display-buffer-reuse-window
                  display-buffer-pop-up-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.2))))

(use-package org-archive
  :after org
  :commands (org-archive-subtree)
  :preface
  (progn
    (autoload 'org-map-entries "org")
    (autoload 'org-set-tags "org")
    (autoload 'org-get-tags "org")

    (defun config-org--archive-done-tasks ()
      (interactive)
      (atomic-change-group
        (org-map-entries (lambda ()
                           ;; HACK: Ensure point does not move past the next
                           ;; item to archive.
                           (let ((org-map-continue-from (point)))
                             (org-archive-subtree)))
                         "/DONE|PAID|VOID|CANCELLED" 'tree)))

    (defun config-org--apply-inherited-tags (&rest _)
      "Apply inherited tags when archiving."
      (org-set-tags (org-get-tags))))

  :config
  (advice-add 'org-archive-subtree :before #'config-org--apply-inherited-tags))

(use-package org-src
  :after org
  :preface
  (progn
    (defun config-org--suppress-final-newline ()
      "Remove trailing newline in src blocks."
      (setq-local require-final-newline nil))

    (defun config-org--org-src-delete-trailing-space (&rest _)
      "Delete trailing whitespace when exiting src blocks."
      (delete-trailing-whitespace)))

  :config
  (progn
    (add-hook 'org-src-mode-hook #'config-org--suppress-final-newline)
    (advice-add 'org-edit-src-exit :before #'config-org--org-src-delete-trailing-space)))

(use-package org-clock
  :after org
  :preface
  (progn
    (autoload 'org-remove-empty-drawer-at "org")

    (defun config-org--remove-empty-clock-drawers ()
      "Remove empty clock drawers at point."
      (save-excursion
        (beginning-of-line 0)
        (org-remove-empty-drawer-at (point)))))

  :config
  (progn
    (org-clock-persistence-insinuate)
    (add-hook 'org-clock-out-hook #'config-org--remove-empty-clock-drawers t)))

(use-package org-crypt
  :after org
  :preface
  (defun config-org--encrypt-on-save ()
    (add-hook 'before-save-hook 'org-encrypt-entries nil t))
  :init
  (add-hook 'org-mode-hook #'config-org--encrypt-on-save))

(use-package evil
  :defer t
  :preface
  (progn
    (autoload 'org-at-heading-p "org")

    (defun config-org--evil-insert-state (&rest _)
      "Enter evil insert state when creating new headings."
      (when (called-interactively-p nil)
        (evil-insert-state)))

    (defun config-org--add-blank-line-after-heading (&rest _)
      "Add a blank line of padding below new headings."
      (when (and (called-interactively-p nil)
                 (org-at-heading-p))
        (let ((next-line-blank?
               (save-excursion
                 (forward-line)
                 (s-blank? (buffer-substring (line-beginning-position) (line-end-position))))))
          (unless next-line-blank?
            (save-excursion
              (goto-char (line-end-position))
              (open-line 1)))))))

  :config
  (progn
    (advice-add 'org-insert-heading :after #'config-org--evil-insert-state)
    (advice-add 'org-insert-heading-respect-content :after #'config-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading-respect-content :after #'config-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading :after #'config-org--evil-insert-state)

    (advice-add 'org-insert-heading :after #'config-org--add-blank-line-after-heading)
    (advice-add 'org-insert-heading-respect-content :after #'config-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading-respect-content :after #'config-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading :after #'config-org--add-blank-line-after-heading)))

(use-package org-present
  :straight t
  :commands (org-present)
  :general
  (:keymaps 'org-present-mode-keymap
   "j"       'scroll-up-line
   "k"       'scroll-down-line
   "p"       'org-present-prev
   "n"       'org-present-next
   "h"       'org-present-prev
   "l"       'org-present-next
   "<left>"  'org-present-prev
   "<up>"    'org-present-prev
   "<right>" 'org-present-next
   "<down>"  'org-present-next
   "SPC"     'org-present-next
   "DEL"     'org-present-prev
   "q"       'org-present-quit
   "<"       'org-present-beginning
   ">"       'org-present-end)
  :preface
  (progn
    (defun config-org--on-org-present-start ()
      (evil-mode -1)
      (vi-tilde-fringe-mode -1)
      (diff-hl-mode -1)
      (org-present-big)
      (org-display-inline-images)
      (org-present-hide-cursor)
      (org-present-read-only)
      (hidden-mode-line-mode +1))

    (defun config-org--on-org-present-end ()
      (evil-mode 1)
      (vi-tilde-fringe-mode 1)
      (diff-hl-mode 1)
      (org-present-small)
      (org-remove-inline-images)
      (org-present-show-cursor)
      (org-present-read-write)
      (hidden-mode-line-mode -1)))

  :config
  (progn
    (setq org-present-text-scale 4)
    (add-hook 'org-present-mode-hook #'config-org--on-org-present-start)
    (add-hook 'org-present-mode-quit-hook #'config-org--on-org-present-end)))

(use-package ox-confluence
  :after org
  :preface
  (autoload 'org-export-define-derived-backend "ox")
  :config
  (org-export-define-derived-backend 'custom-confluence 'confluence
    :menu-entry
    '(?c "Export as Confluence markup"
         ((?c "To temporary buffer" org-confluence-export-as-confluence)))))

(use-package ox-latex
  :after org
  :config
  (add-to-list 'org-latex-minted-langs '(ipython "python")))

(use-package ox-koma-letter
  :after org
  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil)))

(use-package htmlize
  :straight t
  :defer t)

(use-package cb-org-export-koma-letter
  :after org
  :commands (cb-org-export-koma-letter-handler)
  :config
  (progn
    (general-setq org-latex-hyperref-template "")
    (add-to-list 'org-latex-classes `("koma-letter" ,cb-org-export-koma-letter-latex-class))
    (add-hook 'org-ctrl-c-ctrl-c-hook #'cb-org-export-koma-letter-handler t)))

(use-package cb-org-goto
  :commands (cb-org-goto-agenda
             cb-org-goto-diary
             cb-org-goto-notes
             cb-org-goto-todo-list
             cb-org-goto-work
             cb-org-goto-tags-list
             cb-org-goto-headline))

(use-package cb-org-ctrl-c-ret
  :after org
  :general (:states '(normal emacs) :keymaps 'org-mode-map "C-c RET" #'cb-org-ctrl-c-ret))

(use-package cb-org-ctrl-c-ctrl-k
  :after org
  :general
  (:states 'normal :keymaps 'org-mode-map "C-c C-k" #'cb-org-ctrl-c-ctrl-k))

(use-package evil-org
  :straight t
  :after (:and org evil-common)
  :hook (org-mode . evil-org-mode)
  :general
  (:states 'normal :keymaps 'evil-org-mode-map "t" #'org-todo)
  :config
  ;; Remove weird keybindings.
  (general-unbind :states '(normal insert) :keymaps 'evil-org-mode-map
    "M-l" "M-h" "J" "O" "M-l" "M-h"))

(use-package org-chef
  :straight t
  :commands (org-chef-get-recipe-from-url))



(use-package org-indent :hook (org-mode . org-indent-mode))

(use-package org-duration :after org)

(use-package org-download
  :straight t
  :hook (dired-mode . org-download-enable))

(use-package org-html-span :after org)

(use-package cb-org-clock-cascade :hook (org-mode . cb-org-clock-cascade-init))

(use-package cb-diary-utils :after org)

(provide 'config-org)

;;; config-org.el ends here
