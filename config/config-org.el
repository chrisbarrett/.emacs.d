;;; config-org.el --- Configuration for org-mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'general)
(require 'major-mode-hydra)

(use-package org-funcs
  :demand t
  :general
  (:states 'normal :keymaps 'org-mode-map
   "C-c p" #'org-funcs-toggle-priority
   "C-c RET" #'org-funcs-ctrl-c-ret
   "C-c C-k" #'org-funcs-ctrl-c-ctrl-k)
  (:states 'motion :keymaps 'org-agenda-mode-map
   "C-c p" #'org-funcs-agenda-toggle-priority))

(require 'paths)
(require 's)

(autoload 'org-heading-components "org")

;; Silence byte-compiler.

(eval-when-compile
  (defvar org-done-keywords))



(major-mode-hydra-define org-mode nil
  ("Tree"
   (("a" org-archive-subtree "archive")
    ("r" org-refile "refile")
    ("t" org-show-todo-tree "todo tree"))

   "Misc"
   (("e" org-babel/body "babel commands"))))

;; Load ox backends.

(use-package ox-gfm
  :straight t
  :after org)

(use-package ox-slack
  :commands (org-slack-export-to-clipboard-as-slack))

(use-package ox-koma-letter
  :after org
  :defer t
  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil)))

(use-package ob-restclient
  :straight t
  :defer t)

;; General variables

(general-setq
 org-imenu-depth 4
 org-M-RET-may-split-line nil
 org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
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
 org-outline-path-complete-in-steps nil
 org-pretty-entities nil
 org-refile-allow-creating-parent-nodes 'confirm
 org-refile-target-verify-function (lambda () (not (member (nth 2 (org-heading-components)) org-done-keywords)))
 org-refile-use-outline-path t
 org-return-follows-link t
 org-reverse-note-order nil
 org-startup-indented t
 org-startup-with-inline-images t
 org-adapt-indentation nil

 org-log-into-drawer t
 org-log-done 'time
 org-log-redeadline 'time
 org-log-reschedule 'time
 org-log-repeat 'time

 org-todo-keywords '((type "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))

 org-src-fontify-natively t
 org-src-window-setup 'current-window

 ;; I customise priorities so that headlines are either 'prioritised' an show a
 ;; bullet cookie or 'cleared', which indicates no special priority.
 ;;
 ;; org-hacks.el patches the interactive commands to enforce this.
 org-priority-start-cycle-with-default t
 org-highest-priority ?A
 org-lowest-priority ?B
 org-default-priority ?B


 ;; org-babel

 org-confirm-babel-evaluate nil

 ;; org-export

 org-export-backends '(ascii html latex odt slack gfm koma-letter)
 org-html-html5-fancy t
 org-html-postamble nil
 org-export-exclude-tags '("noexport" "no_export" "crypt" "ignore")
 org-export-coding-system 'utf-8

 ;; latex

 org-latex-compiler "tectonic"
 org-latex-pdf-process '("tectonic --outdir %o %f")

 ;; org-habit

 org-habit-graph-column 68
 org-habit-preceding-days 7
 org-habit-following-days 5

 org-tags-exclude-from-inheritance '("crypt" "project")
 org-crypt-disable-auto-save 'encypt

 org-attach-directory (f-join org-directory "data")
 org-archive-location "archive.org::datetree/"
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

 ;; agenda
 org-stuck-projects '("+project-ignore-@someday-archived" ("TODO") nil "")
 org-agenda-auto-exclude-function #'org-funcs-exclude-tasks-on-hold
 org-agenda-hide-tags-regexp (rx (or "noexport" "@someday" "project"))
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
 org-agenda-use-time-grid nil)

(general-setq org-agenda-custom-commands
              (let ((contexts '("@personal" "@work")))
                (-mapcat (lambda (tag)
                           `((,(concat (substring tag 1 2) "a")
                              ,(format "Agenda for context: %s" tag)
                              ((agenda ""
                                       ((org-agenda-overriding-header "Today")
                                        (org-agenda-use-time-grid t)))
                               (tags-todo "TODO=\"TODO\"|+PRIORITY=\"A\""
                                          ((org-agenda-overriding-header "Next Actions")
                                           (org-agenda-skip-function #'org-funcs-skip-items-already-in-agenda)))
                               (todo "WAITING"
                                     ((org-agenda-overriding-header "Delegated")
                                      (org-agenda-skip-function #'org-funcs-skip-item-if-timestamp)))
                               (stuck ""
                                      ((org-agenda-overriding-header "Stuck Projects"))))
                              ((org-agenda-tag-filter-preset '(,(format "+%s" tag) "-@someday" "-ignore"))
                               (org-agenda-span 'day)
                               (org-agenda-show-future-repeats nil)
                               (org-agenda-archives-mode nil)
                               (org-agenda-ignore-drawer-properties '(effort appt))))

                             (,(concat (substring tag 1 2) "p")
                              ,(format "Plan for context: %s" tag)
                              ((agenda ""
                                       ((org-agenda-overriding-header "Review agenda this week")
                                        (org-agenda-use-time-grid t)))
                               (todo "WAITING"
                                     ((org-agenda-overriding-header "Review Delegated Actions. Should I follow up today or course correct?")))
                               (todo "TODO"
                                     ((org-agenda-overriding-header "Review Next Actions. Are these the next thing to do?")
                                      (org-agenda-skip-function #'org-funcs-skip-items-already-in-agenda)))
                               (tags-todo "+LEVEL=1+TODO=\"TODO\""
                                          ((org-agenda-overriding-header "Review unscheduled actions. Any of these need to be prioritised?")
                                           (org-agenda-skip-function #'org-funcs-skip-item-if-timestamp)))
                               (tags "+project-archived"
                                     ((org-agenda-overriding-header "Review projects. Are these all healthy?"))))
                              ((org-agenda-tag-filter-preset '(,(format "+%s" tag) "-@someday" "-ignore"))
                               (org-agenda-span 'week)
                               (org-agenda-show-future-repeats nil)
                               (org-agenda-archives-mode nil)
                               (org-agenda-ignore-drawer-properties '(effort appt))))

                             (,(concat (substring tag 1 2) "r")
                              ,(format "Review for context: %s" tag)
                              ((agenda ""
                                       ((org-agenda-overriding-header "Review agenda this week")
                                        (org-agenda-show-log t)
                                        (org-agenda-start-day "-7d")))
                               (todo "TODO"
                                     ((org-agenda-overriding-header "Review Next Actions. Are these the next thing to do?")
                                      (org-agenda-skip-function #'org-funcs-skip-items-already-in-agenda)))
                               (tags-todo "+LEVEL=1+TODO=\"TODO\""
                                          ((org-agenda-overriding-header "Review unscheduled actions. Any of these need to be prioritised?")
                                           (org-agenda-skip-function #'org-funcs-skip-item-if-timestamp)))
                               (tags "+project-archived"
                                     ((org-agenda-overriding-header "Review projects. Are these all healthy?"))))
                              ((org-agenda-tag-filter-preset '(,(format "+%s" tag) "-@someday" "-ignore"))
                               (org-agenda-span 14)
                               (org-agenda-show-future-repeats nil)
                               (org-agenda-archives-mode nil)
                               (org-agenda-ignore-drawer-properties '(effort appt))))))
                         contexts)))

(defconst config-org-capture-templates
  (list
   (org-funcs-capture-template
    "t" "Todo" '(file "inbox.org") "* TODO %?")

   '("n" "Note" entry
     (file+function org-default-notes-file org-reverse-datetree-goto-date-in-file)
     "* %?" :tree-type week)

   `("r" "Weekly Review" entry
     (file+function org-default-notes-file org-reverse-datetree-goto-date-in-file)
     (file ,(f-join org-directory "templates" "review.template.org"))
     :tree-type week)

   (org-funcs-capture-template
    "l" "Link" '(file "inbox.org") '(function cb-org-capture-url-read-url)
    :immediate-finish t)))



(use-package org
  :defer t
  :straight t
  :general
  ("C-c a" #'org-agenda
   "C-c s" #'org-search-view
   "C-c t" #'org-todo-list
   "C-c /" #'org-tags-view)
  (:states '(emacs normal) :keymaps 'org-mode-map
   "<backtab>" #'org-global-cycle
   "C-c C-." #'org-time-stamp-inactive
   "C-c c" #'org-columns
   "M-n" #'org-metadown
   "M-p" #'org-metaup
   "RET" #'org-return)

  :preface
  (progn
    (autoload 'org-todo "org")
    (autoload 'org-up-heading-safe "org")

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

    (defun config-org--set-local-vars-and-hooks ()
      (add-hook 'org-after-todo-state-change-hook #'config-org--mark-next-parent-tasks-todo nil t)
      (add-hook 'org-clock-in-hook #'config-org--mark-next-parent-tasks-todo nil t))

    (defun config-org--children-done-parent-done (_n-done n-todo)
      "Mark the parent task as done when all children are completed."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (zerop n-todo) "DONE" "TODO")))))

  :hook (org-mode . visual-line-mode)

  :commands
  (org-refile)

  :init
  (progn
    (add-to-list 'load-path (expand-file-name "lisp" org-directory))
    (add-hook 'org-mode-hook #'auto-revert-mode)
    (add-hook 'org-mode-hook #'config-org--set-local-vars-and-hooks)
    (add-hook 'org-mode-hook #'config-org--set-bidi-env)
    (add-hook 'org-after-todo-statistics-hook #'config-org--children-done-parent-done))

  :config
  (general-setq
   org-default-notes-file (f-join org-directory "notes.org")
   org-agenda-files (f-files org-directory (lambda (f) (f-ext? f "org")))

   org-refile-use-outline-path 'file
   org-refile-targets
   '((org-agenda-files . (:maxlevel . 3))
     (org-default-notes-file :maxlevel . 3)
     (org-directory :maxlevel . 3)))

  :config
  (progn
    ;; Configure capture templates
    (setq org-capture-templates config-org-capture-templates)
    (let ((custom-templates-initfile (f-join paths-org-templates-directory "init.el")))
      (when (file-exists-p custom-templates-initfile)
        (load-file custom-templates-initfile)))

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

(use-package cb-org-pgp-decrpyt :hook (org-mode . cb-org-pgp-decrpyt-init))

(use-package org-hydras :commands (org-babel/body))

(use-package org-agenda
  :defer t
  :after org
  :general
  (:keymaps 'org-agenda-mode-map :states 'motion
   "<f9>" (lambda ()
            (interactive)
            (org-save-all-org-buffers)
            (org-agenda-redo))
   "/" #'org-agenda-filter-by-tag
   "v" #'org-agenda-view-mode-dispatch
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
      (page-break-lines--update-display-tables)))

  :config
  (progn
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

    ;; Ensure the separator line is rendered whenever the org agenda view
    ;; changes. This is needed for page-break-lines to render the separator
    ;; correctly.
    (setq org-agenda-block-separator "")
    (advice-add 'org-agenda :after #'config-org--draw-separator)
    (advice-add 'org-agenda-redo :after #'config-org--draw-separator)))

(use-package org-archive
  :after org
  :defer t
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
  :defer t
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
  :defer t
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
  :defer t
  :preface
  (defun config-org--encrypt-on-save ()
    (add-hook 'before-save-hook 'org-encrypt-entries nil t))
  :init
  (add-hook 'org-mode-hook #'config-org--encrypt-on-save))

;; evil-org provides better compatability with org-mode.
(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode)
  :general
  (:states 'normal :keymaps 'evil-org-mode-map "t" #'org-todo)
  :config
  ;; Remove weird keybindings.
  (general-unbind :states '(normal insert) :keymaps 'evil-org-mode-map
    "M-l" "M-h" "J" "O" "M-l" "M-h"))

;; org-bullets displays orgmode bullets using pretty utf-8 characters.
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

(with-eval-after-load 'evil

  (defun config-org--evil-insert-state (&rest _)
    "Enter evil insert state when creating new headings."
    (when (called-interactively-p nil)
      (evil-insert-state)))

  (advice-add 'org-insert-heading :after #'config-org--evil-insert-state)
  (advice-add 'org-insert-heading-respect-content :after #'config-org--evil-insert-state)
  (advice-add 'org-insert-todo-heading-respect-content :after #'config-org--evil-insert-state)
  (advice-add 'org-insert-todo-heading :after #'config-org--evil-insert-state))

;; org-reverse-datetree provides a more customisable way of creating date trees.
(use-package org-reverse-datetree
  :straight t
  :commands (org-reverse-datetree-goto-date-in-file)
  :config
  (setq-default org-reverse-datetree-level-formats
                '("%Y-%m %B"
                  "[%Y-%m-%d %a]")))

(use-package cb-org-export-koma-letter
  :after org
  :commands (cb-org-export-koma-letter-handler)
  :config
  (progn
    (general-setq org-latex-hyperref-template "")
    (add-to-list 'org-latex-classes `("koma-letter" ,cb-org-export-koma-letter-latex-class))
    (add-hook 'org-ctrl-c-ctrl-c-hook #'cb-org-export-koma-letter-handler t)))

(provide 'config-org)

;;; config-org.el ends here
