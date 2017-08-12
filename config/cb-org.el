;;; cb-org.el --- Orgmode configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions)
  (require 'cb-emacs)
  (autoload 'evil-define-key "evil-core")
  (defconst cb-org-load-path (concat cb-emacs-lisp-directory "/org-mode/lisp"))
  (defconst cb-org-contrib-load-path (concat cb-emacs-lisp-directory "/org-mode/contrib/lisp")))

(require 'spacemacs-keys)
(require 'evilified-state)
(require 'f)
(require 's)
(require 'dash)
(require 'subr-x)

(defvar org-directory "~/org")

(defconst cb-org-work-file (concat org-directory "/work_movio.org"))

(with-eval-after-load 'which-key
  (with-no-warnings
    (push `(("," . ,(rx bos (? "evil-") "org-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package org
  :load-path cb-org-load-path
  :defer t

  :bind
  (:map org-mode-map
        ("C-c C-." . org-time-stamp-inactive)
        ("M-p" . org-metaup)
        ("M-n" . org-metadown)
        ("C-c c" . org-columns))

  :evil-bind
  (:map org-mode-map :state normal ("RET" . org-return))

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

    (defun cb-org--exit-minibuffer (&rest _)
      "Exit minibuffer before adding notes."
      (when (minibufferp (window-buffer (selected-window)))
        (other-window 1)))

    (defun cb-org--toggle-heading-goto-eol (&rest _)
      "Prevent point from moving to BOL when toggling headings."
      (when (s-matches? (rx bol (+ "*") (* space) eol)
                        (buffer-substring (line-beginning-position) (line-end-position)))
        (goto-char (line-end-position))))

    (defun cb-org--mark-next-parent-tasks-todo ()
      "Visit each parent task and change state to TODO."
      (when-let (mystate (or (bound-and-true-p org-state)
                             (nth 2 (org-heading-components))))
        (save-excursion
          (while (org-up-heading-safe)
            (when (-contains? '("WAITING" "MAYBE")
                              (nth 2 (org-heading-components)))
              (org-todo "TODO"))))))

    (defun cb-org--add-local-hooks ()
      "Set buffer-local hooks for orgmode."
      (add-hook 'org-after-todo-state-change-hook #'cb-org--mark-next-parent-tasks-todo nil t)
      (add-hook 'org-clock-in-hook #'cb-org--mark-next-parent-tasks-todo nil t))

    (defun cb-org--children-done-parent-done (_n-done n-todo)
      "Mark the parent task as done when all children are completed."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (zerop n-todo) "DONE" "TODO")))))

  :commands
  (org-refile)

  :init
  (progn
    (add-hook 'org-mode-hook #'cb-org--add-local-hooks)
    (add-hook 'org-after-todo-statistics-hook #'cb-org--children-done-parent-done)

    (dolist (dir (f-directories "~/org/lisp/"))
      (add-to-list 'load-path (f-slash dir)))

    (spacemacs-keys-set-leader-keys
      "ok" #'org-capture
      "ol" #'org-store-link
      "os" #'org-search-view))

  :config
  (progn

    (setq org-default-notes-file (f-join org-directory "notes.org"))

    (spacemacs-keys-set-leader-keys-for-major-mode
      'org-mode
      "r" #'org-refile)

    (add-to-list 'org-refile-targets '(nil :maxlevel . 3))
    (add-to-list 'org-refile-targets '(org-default-notes-file :maxlevel . 3))
    (add-to-list 'org-refile-targets `(,org-directory :maxlevel . 3))
    (add-to-list 'org-tags-exclude-from-inheritance "project")

    (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file-other-window)

    (setq org-M-RET-may-split-line nil)
    (setq org-catch-invisible-edits 'smart)
    (setq org-cycle-separator-lines 1)
    (setq org-enforce-todo-dependencies t)
    (setq org-footnote-auto-adjust t)
    (setq org-indirect-buffer-display 'current-window)
    (setq org-insert-heading-respect-content t)
    (setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-hide-emphasis-markers t)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-pretty-entities t)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-target-verify-function (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords))))

    (setq org-refile-use-outline-path t)
    (setq org-return-follows-link t)
    (setq org-reverse-note-order nil)
    (setq org-confirm-elisp-link-function nil)
    (setq org-startup-indented t)
    (setq org-startup-with-inline-images t)
    (setq org-hierarchical-todo-statistics nil)
    (setq org-checkbox-hierarchical-statistics t)
    (setq org-log-repeat nil)
    (setq org-blank-before-new-entry '((heading . always) (plain-list-item . nil)))

    (setq org-todo-keywords '((type "TODO(t)" "MAYBE(m)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
                              (type "SOMEDAY(s)" "|")))


    (setq org-confirm-babel-evaluate nil)
    (setq org-babel-load-languages '((emacs-lisp . t)
                                     (restclient . t)
                                     (python . t)
                                     (ipython . t)
                                     (scala . t)
                                     (shell . t)
                                     (sql . t)))

    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

    (advice-add 'org-add-log-note :before #'cb-org--exit-minibuffer)
    (advice-add 'org-toggle-heading :after #'cb-org--toggle-heading-goto-eol)))

(use-package ob-ipython
  :after org
  :preface
  (progn
    (define-derived-mode org-babel-traceback-mode special-mode "traceback")
    (evil-set-initial-state 'org-babel-traceback-mode 'motion)

    (defun cb-org--setup-traceback-buffer (buf)
      (with-current-buffer buf
        (org-babel-traceback-mode))
      buf)

    (defun cb-org--setup-ipython (f &rest args)
      (if (executable-find "ipython")
          (let ((python-shell-interpreter "ipython")
                (python-shell-interpreter-args "--simple-prompt -i")
                (python-shell-prompt-detect-failure-warning nil))
            (apply f args))
        (apply f args))))
  :config
  (progn
    ;; HACK: ipython-mode must be defined for source blocks to work.
    (defalias 'ipython-mode 'python-mode)

    (advice-add 'org-babel-execute:ipython :around #'cb-org--setup-ipython)
    (advice-add 'ob-ipython--create-traceback-buffer :filter-return #'cb-org--setup-traceback-buffer)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*ob-ipython-traceback*" eos)
                   (display-buffer-reuse-window
                    display-buffer-pop-up-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.5)))))

(use-package ob-python
  :after org
  :preface
  (defun cb-org-setup-python ()
    (when (executable-find "ipython")
      (setq-local org-babel-python-command "ipython")))
  :config
  (add-hook 'org-mode-hook #'cb-org-setup-python))

(use-package ob-restclient :after org)

(use-package ob-shell :after org)

(use-package ob-sql :after org)

(use-package org-id
  :after org
  :config
  (setq org-id-locations-file (f-join cb-emacs-cache-directory "org-id-locations")))

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

(use-package cb-org-hydras
  :after org
  :init
  (progn
    (with-eval-after-load 'which-key
      (with-no-warnings
        (push `((nil . ,(rx bos "cb-org-babel/body")) . (nil . "babel"))
              which-key-replacement-alist)))
    (spacemacs-keys-set-leader-keys-for-major-mode 'org-mode
      "e" 'cb-org-babel/body)))


(use-package org-attach
  :after org
  :config
  (setq org-attach-directory (f-join org-directory "data")))

(use-package org-agenda
  :load-path cb-org-load-path
  :after org
  :bind (:map org-agenda-mode-map ("J" . org-agenda-goto-date))

  :preface
  (progn

    (defun cb-org--exclude-tasks-on-hold (tag)
      (and (equal tag "hold") (concat "-" tag)))

    (defun cb-org--agenda-skip-if-has-timestamp ()
      "Skip the item if it has a scheduled or deadline timestamp."
      (when (or (org-get-scheduled-time (point))
                (org-get-deadline-time (point)))
        (or (outline-next-heading)
            (goto-char (point-max)))))

    (defun cb-org--current-headline-is-todo ()
      (string= "TODO" (org-get-todo-state)))

    (defun cb-org--agenda-skip-all-siblings-but-first ()
      "Skip all but the first non-done entry."
      (let (should-skip-entry)
        (unless (cb-org--current-headline-is-todo)
          (setq should-skip-entry t))
        (save-excursion
          (while (and (not should-skip-entry) (org-goto-sibling t))
            (when (cb-org--current-headline-is-todo)
              (setq should-skip-entry t))))
        (when should-skip-entry
          (or (outline-next-heading)
              (goto-char (point-max)))))))

  :bind
  ("C-c a" . org-agenda)

  :config
  (progn
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      (kbd "M-j") 'org-agenda-next-item
      (kbd "M-k") 'org-agenda-previous-item
      (kbd "M-h") 'org-agenda-earlier
      (kbd "M-l") 'org-agenda-later
      (kbd "gd") 'org-agenda-toggle-time-grid
      (kbd "gr") 'org-agenda-redo
      (kbd "M-RET") 'org-agenda-show-and-scroll-up)


    (define-key org-agenda-mode-map (kbd "C-f" ) #'evil-scroll-page-down)
    (define-key org-agenda-mode-map (kbd "C-b") #'evil-scroll-page-up)

    ;; Match projects that do not have any todos.
    (setq org-stuck-projects '("+project-ignore-maybe-done"
                               ("TODO") nil))

    ;; Enable leader key in agenda.
    (define-key org-agenda-mode-map (kbd "SPC") spacemacs-keys-default-map)

    (setq org-agenda-include-diary nil)
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-auto-exclude-function #'cb-org--exclude-tasks-on-hold)
    (setq org-agenda-files (f-files org-directory (lambda (f) (f-ext? f "org"))))
    (setq org-agenda-diary-file (f-join org-directory "diary.org"))
    (setq org-agenda-hide-tags-regexp (rx (or "noexport" "someday" "project")))
    (setq org-agenda-insert-diary-extract-time t)
    (setq org-agenda-span 'week)
    (setq org-agenda-search-view-always-boolean t)
    (setq org-agenda-show-all-dates nil)
    (setq org-agenda-show-inherited-tags nil)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-sorting-strategy
          '((agenda time-up priority-down category-keep)
            (todo priority-down category-keep scheduled-up)
            (tags priority-down category-keep)
            (search category-keep)))
    (setq org-agenda-text-search-extra-files '(agenda-archives))
    (setq org-agenda-use-time-grid nil)
    (setq org-agenda-inhibit-startup t)
    (setq org-agenda-tags-column -100)

    (setq org-agenda-clockreport-parameter-plist
          (list
           :compact t
           :maxlevel 5
           :fileskip0 t
           :step 'week))

    (add-hook 'org-finalize-agenda-hook #'org-agenda-to-appt)

    (setq org-agenda-custom-commands
          '(("A" "Agenda and next actions"
             ((tags-todo "-study-someday-media-gtd/TODO"
                         ((org-agenda-overriding-header "Next Actions")
                          ;; Take the first item from each todo list. Also
                          ;; exclude items with scheduled/deadline times, since
                          ;; they show up in the calendar views.
                          (org-agenda-skip-function (lambda ()
                                                      (or (cb-org--agenda-skip-if-has-timestamp)
                                                          (cb-org--agenda-skip-all-siblings-but-first))))))
              (agenda "")
              (todo "WAITING"
                    ((org-agenda-overriding-header "Waiting")))
              (stuck "")
              (tags-todo "media|study/TODO"
                         ((org-agenda-overriding-header "Media & Study"))))
             ((org-agenda-tag-filter-preset '("-ignore"))
              (org-agenda-files (list org-default-notes-file org-agenda-diary-file))
              (org-agenda-dim-blocked-tasks nil)
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
                          (org-agenda-skip-function #'cb-org--agenda-skip-all-siblings-but-first)))
              (tags-todo "+goals+3_months+project/TODO"
                         ((org-agenda-overriding-header "Review 3 Month Goals")
                          (org-agenda-skip-function #'cb-org--agenda-skip-all-siblings-but-first)))
              (tags-todo "+goals+1_year+project/TODO"
                         ((org-agenda-overriding-header "Review 1 Year Goals")
                          (org-agenda-skip-function #'cb-org--agenda-skip-all-siblings-but-first)))
              (tags-todo "+goals+3_years+project/MAYBE|SOMEDAY|TODO"
                         ((org-agenda-overriding-header "Review 3 Year Goals")
                          (org-agenda-skip-function #'cb-org--agenda-skip-all-siblings-but-first)))
              (tags-todo "someday-skill/MAYBE|TODO"
                         ((org-agenda-overriding-header "Decide whether to promote any SOMEDAY items to TODOs")))
              (tags-todo "someday&skill"
                         ((org-agenda-overriding-header "Decide whether to promote any learning tasks to TODOs"))))
             ((org-agenda-tag-filter-preset
               '("-drill" "-gtd" "-ignore"))
              (org-agenda-include-inactive-timestamps t)
              (org-agenda-files (list org-default-notes-file cb-org-work-file org-agenda-diary-file))
              (org-agenda-archives-mode nil)
              (org-agenda-dim-blocked-tasks nil)))

            ("w" "Work actions"
             ((tags-todo "-study-someday-media-gtd/TODO"
                         ((org-agenda-overriding-header "Next Actions")
                          (org-agenda-skip-function #'cb-org--agenda-skip-all-siblings-but-first)))
              (todo "WAITING"
                    ((org-agenda-overriding-header "Waiting")))
              (stuck "")
              (agenda "")
              (tags "+standup"
                    ((org-agenda-overriding-header "Standup"))))
             ((org-agenda-tag-filter-preset '("-ignore"))
              (org-agenda-use-tag-inheritance nil)
              (org-agenda-files (list cb-org-work-file org-agenda-diary-file))
              (org-agenda-dim-blocked-tasks nil)
              (org-agenda-archives-mode nil)
              (org-agenda-ignore-drawer-properties '(effort appt))))))))

(use-package appt
  :defer t
  :config
  (progn
    (setq appt-message-warning-time 60)
    (setq appt-display-interval 5)))

(use-package org-archive
  :after org
  :load-path cb-org-load-path
  :functions (org-archive-subtree)
  :preface
  (progn
    (autoload 'org-map-entries "org")
    (autoload 'org-set-tags-to "org")
    (autoload 'org-get-tags-at "org")

    (defun cb-org--archive-done-tasks ()
      (interactive)
      (atomic-change-group
        (org-map-entries (lambda ()
                           ;; HACK: Ensure point does not move past the next
                           ;; item to archive.
                           (let ((org-map-continue-from (point)))
                             (org-archive-subtree)))
                         "/DONE|PAID|VOID|CANCELLED" 'tree)))

    (defun cb-org--apply-inherited-tags (&rest _)
      "Apply inherited tags when archiving."
      (org-set-tags-to (org-get-tags-at))))

  :config
  (progn
    (setq org-archive-default-command #'cb-org--archive-done-tasks)
    (advice-add 'org-archive-subtree :before #'cb-org--apply-inherited-tags)))

(use-package org-src
  :after org
  :load-path cb-org-load-path

  :preface
  (progn
    (defun cb-org--suppress-final-newline ()
      "Remove trailing newline in src blocks."
      (setq-local require-final-newline nil))

    (defun cb-org--org-src-delete-trailing-space (&rest _)
      "Delete trailing whitespace when exiting src blocks."
      (delete-trailing-whitespace)))

  :config
  (progn
    (setq org-src-fontify-natively t)
    (setq org-src-window-setup 'current-window)
    (add-hook 'org-src-mode-hook #'cb-org--suppress-final-newline)
    (advice-add 'org-edit-src-exit :before #'cb-org--org-src-delete-trailing-space)))

(use-package org-clock
  :after org
  :load-path cb-org-load-path

  :preface
  (progn
    (autoload 'org-remove-empty-drawer-at "org")

    (defun cb-org--remove-empty-clock-drawers ()
      "Remove empty clock drawers at point."
      (save-excursion
        (beginning-of-line 0)
        (org-remove-empty-drawer-at (point)))))

  :config
  (progn
    (setq org-clock-persist t)
    (setq org-clock-persist-query-resume nil)
    (setq org-clock-history-length 20)
    (setq org-clock-in-resume t)
    (setq org-clock-report-include-clocking-task t)
    (setq org-clock-out-remove-zero-time-clocks t)
    (setq org-clock-persist-file (f-join org-directory ".org-clock-save"))

    (org-clock-persistence-insinuate)
    (add-hook 'org-clock-out-hook #'cb-org--remove-empty-clock-drawers t)))

(use-package org-crypt
  :after org
  :load-path cb-org-load-path
  :functions (org-encrypt-entries)
  :preface
  (defun cb-org--encrypt-on-save ()
    (add-hook 'before-save-hook #'org-encrypt-entries nil t))
  :init
  (add-hook 'org-mode-hook #'cb-org--encrypt-on-save)
  :config
  (progn
    (setq org-crypt-disable-auto-save 'encypt)
    (add-to-list 'org-tags-exclude-from-inheritance "crypt")))

(use-package org-drill
  :after org
  :commands (org-drill
             org-drill-strip-all-data
             org-drill-cram
             org-drill-tree
             org-drill-resume
             org-drill-merge-buffers
             org-drill-entry
             org-drill-directory
             org-drill-again)
  :preface
  (defconst cb-org-drill-files (f-files (concat org-directory "/drill")))

  :defines
  (org-drill-scope
   org-drill-learn-fraction
   org-drill-adjust-intervals-for-early-and-late-repetitions-p
   org-drill-add-random-noise-to-intervals-p
   org-drill-save-buffers-after-drill-sessions-p)

  :config
  (progn
    (defconst cb-org-drill-file (f-join org-directory "drill" "drill.org"))

    (setq org-drill-scope cb-org-drill-files)

    (add-to-list 'org-refile-targets '(cb-org-drill-files :maxlevel . 3))

    (setq org-drill-learn-fraction 0.25)
    (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
    (setq org-drill-add-random-noise-to-intervals-p t)
    (setq org-drill-save-buffers-after-drill-sessions-p nil)))

(use-package org-capture
  :after org
  :load-path cb-org-load-path
  :preface
  (defun cb-org--capture-template-entry (key label form template &rest kws)
    (append
     (list key label 'entry form template
           :clock-keep t
           :empty-lines 1
           :prepend t)
     kws))
  :config
  (setq org-capture-templates
        (list
         (cb-org--capture-template-entry
          "t" "Todo"
          '(file org-default-notes-file) "* TODO %?")

         (cb-org--capture-template-entry
          "T" "Todo (work)"
          `(file ,cb-org-work-file) "* TODO %?")

         (cb-org--capture-template-entry
          "d" "Diary"
          '(file+datetree org-agenda-diary-file) "* %?\n%^t")

         (cb-org--capture-template-entry
          "D" "Diary (work)"
          `(file+datetree cb-org-work-file) "* %?\n%^t")

         (cb-org--capture-template-entry
          "l" "Link"
          '(file+olp org-default-notes-file "Links")
          '(function cb-org-capture-url-read-url)
          :immediate-finish t)

         (cb-org--capture-template-entry
          "L" "Link (work)"
          `(file+olp cb-org-work-file "Links")
          '(function cb-org-capture-url-read-url)
          :immediate-finish t)

         (cb-org--capture-template-entry
          "s" "Someday"
          '(file+olp org-default-notes-file "Someday")
          "* SOMEDAY %?")

         (cb-org--capture-template-entry
          "m" "Listening"
          '(file+olp org-default-notes-file "Media" "Listening")
          "* MAYBE Listen to %i%?")

         (cb-org--capture-template-entry
          "v" "Viewing"
          '(file+olp org-default-notes-file "Media" "Viewing")
          "* MAYBE Watch %i%?")

         (cb-org--capture-template-entry
          "r" "Reading"
          '(file+olp org-default-notes-file "Media" "Reading")
          "* MAYBE Read %i%?")

         (cb-org--capture-template-entry
          "0" "Drill (item)"
          '(file+olp cb-org-drill-file "Uncategorised")
          "* Item                :drill:

%?
"
          :jump-to-captured t)

         (cb-org--capture-template-entry
          "1" "Drill (question)"
          '(file+olp cb-org-drill-file "Uncategorised")
          "* Question                :drill:

%?

** Answer
"
          :jump-to-captured t)

         (cb-org--capture-template-entry
          "2" "Drill (two-sided)"
          '(file+olp cb-org-drill-file "Uncategorised")
          "* Question                :drill:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:

%?

** Side 1

** Side 2
"
          :jump-to-captured t)

         (cb-org--capture-template-entry
          "e" "Email task"
          '(file org-default-notes-file) "* TODO %?\n%a")

         (cb-org--capture-template-entry
          "E" "Email task (work)"
          `(file cb-org-work-file) "* TODO %?\n%a"))))

(use-package org-download
  :after org
  :load-path cb-org-load-path
  :config
  (setq org-download-method 'attach))

(use-package evil
  :defer t
  :preface
  (progn
    (autoload 'org-at-heading-p "org")

    (defun cb-org--evil-insert-state (&rest _)
      "Enter evil insert state when creating new headings."
      (when (called-interactively-p nil)
        (evil-insert-state)))

    (defun cb-org--add-blank-line-after-heading (&rest _)
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
    (advice-add 'org-insert-heading :after #'cb-org--evil-insert-state)
    (advice-add 'org-insert-heading-respect-content :after #'cb-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading-respect-content :after #'cb-org--evil-insert-state)
    (advice-add 'org-insert-todo-heading :after #'cb-org--evil-insert-state)

    (advice-add 'org-insert-heading :after #'cb-org--add-blank-line-after-heading)
    (advice-add 'org-insert-heading-respect-content :after #'cb-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading-respect-content :after #'cb-org--add-blank-line-after-heading)
    (advice-add 'org-insert-todo-heading :after #'cb-org--add-blank-line-after-heading)))

(use-package org-present
  :commands (org-present)
  :config
  (setq org-present-text-scale 4))

(use-package ox
  :after org
  :init
  (defvar org-export-backends '(ascii html latex odt gfm koma-letter custom-confluence))
  :config
  (progn
    (setq org-export-exclude-tags '("noexport" "crypt"))
    (setq org-export-coding-system 'utf-8)))

(use-package ox-confluence
  :after org
  :preface
  (autoload 'org-export-define-derived-backend "ox")
  :config
  (org-export-define-derived-backend 'custom-confluence 'confluence
    :menu-entry
    '(?c "Export as Confluence markup"
         ((?c "To temporary buffer" org-confluence-export-as-confluence)))))

(use-package ox-gfm :after org)

(use-package ox-latex
  :after org
  :config
  (add-to-list 'org-latex-minted-langs '(ipython "python")))

(use-package ox-koma-letter
  :after org
  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil)))

(use-package ox-html
  :after org
  :preface
  (defun cb-org-html-open-tags-setup
      (number _group-number _start-group-p _end-group-p topp bottomp)
    (cond (topp "<tr class=\"tr-top\">")
          (bottomp "<tr class=\"tr-bottom\">")
          (t (if (= (mod number 2) 1)
                 "<tr class=\"tr-odd\">"
               "<tr class=\"tr-even\">"))))
  :config
  (progn
    (setq org-html-html5-fancy t)
    (setq org-html-postamble nil)

    ;; Highlight alternating rows in HTML tables.

    (setq org-html-table-row-open-tag #'cb-org-html-open-tags-setup)
    (setq org-html-head-extra
          "
<style type=\"text/css\">
table tr.tr-odd td {
      background-color: #FCF6CF;
}
table tr.tr-even td {
      background-color: #FEFEF2;
}
</style>
")))

(use-package cb-org-clock-cascade
  :after org
  :functions (cb-org-clock-cascade-init)
  :init (add-hook 'org-mode-hook #'cb-org-clock-cascade-init))

(use-package cb-org-export-koma-letter
  :after org
  :commands (cb-org-export-koma-letter-handler)
  :config
  (progn
    (add-to-list 'org-latex-classes `("koma-letter" ,cb-org-export-koma-letter-latex-class))
    (setq org-latex-hyperref-template "")
    (add-hook 'org-ctrl-c-ctrl-c-hook #'cb-org-export-koma-letter-handler t)))

(use-package cb-org-pgp-decrpyt
  :after org
  :functions (cb-org-pgp-decrpyt-init)
  :init (add-hook 'org-mode-hook #'cb-org-pgp-decrpyt-init))

(use-package cb-org-capture-url
  :after org)

(use-package cb-org-gdrive
  :after org
  :functions (cb-org-gdrive-init)
  :init (add-hook 'org-mode-hook #'cb-org-gdrive-init))

(use-package cb-org-goto
  :commands (cb-org-goto-agenda
             cb-org-goto-diary
             cb-org-goto-notes
             cb-org-goto-work
             cb-org-goto-todo-list
             cb-org-goto-tags-list)
  :init
  (spacemacs-keys-set-leader-keys
    "oa" #'cb-org-goto-agenda
    "od" #'cb-org-goto-diary
    "on" #'cb-org-goto-notes
    "ow" #'cb-org-goto-work
    "ot" #'cb-org-goto-todo-list
    "ov" #'cb-org-goto-tags-list))

(use-package cb-org-ctrl-c-ret
  :after org
  :evil-bind (:map org-mode-map
                   :state normal ("C-c RET" . cb-org-ctrl-c-ret)
                   :state emacs ("C-c RET" . cb-org-ctrl-c-ret)))

(use-package cb-org-ctrl-c-ctrl-k
  :after org
  :evil-bind (:map org-mode-map :state normal ("C-c C-k" . cb-org-ctrl-c-ctrl-k)))

(use-package cb-diary-utils
  :after org)

(use-package evil-org
  :after org
  :evil-bind
  (:map evil-org-mode-map
        :state normal
        ("M-l" . nil)
        ("M-h" . nil)
        :state insert
        ("M-l" . nil)
        ("M-h" . nil))
  :config
  (progn
    ;; Remove weird keybindings.
    (evil-define-key 'normal evil-org-mode-map (kbd "J") nil)
    (evil-define-key 'normal evil-org-mode-map (kbd "O") nil)))

(use-package org-indent
  :after org
  :commands (org-indent-mode)
  :init
  (add-hook 'org-mode-hook #'org-indent-mode))


(provide 'cb-org)

;;; cb-org.el ends here
