;;; config-org.el --- Configuration for org-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs ships with a (generally outdated) version of org. This makes trying to
;; install another version difficult.
;;
;; I use a custom Emacs build that strips out the built-in org and replaces it
;; with a more recent version.

;;; Code:


(eval-when-compile
  (require 'use-package))

(cl-eval-when (compile)
  (require 'evil)
  (require 'org)
  (require 'company)
  (require 'org-edna))

(require 'f)
(require 'general)
(require 'major-mode-hydra)
(require 'memoize)
(require 'org-funcs)
(require 'paths)
(require 's)

(autoload 'hide-header-line-mode "hide-header-line")
(autoload 'org-indent-mode "org-indent")
(autoload 'org-present "org-present")
(autoload 'org-project-skip-non-projects "org-project")
(autoload 'org-project-skip-non-stuck-projects "org-project")



(major-mode-hydra-define+ org-mode nil
  ("Tree"
   (("a" org-archive-subtree "archive")
    ("r" org-funcs-refile-dwim "refile")
    ("x" org-cut-subtree "cut")
    ("y" org-copy-subtree "copy")
    ("p" org-paste-subtree "paste"))
   "View"
   (("P" org-present "present")
    ("n" org-num-mode "heading numbers")
    ("o" org-tree-to-indirect-buffer "tree to indirect buf")
    ("t" org-show-todo-tree "todo tree"))
   "Misc"
   (("b" org-edna-edit "edit blockers & triggers")
    ("e" org-babel/body "babel commands"))))

(major-mode-hydra-define+ org-agenda-mode nil
  ("Tree"
   (("a" org-agenda-archive "archive")
    ("r" org-funcs-refile-dwim "refile"))))

;; Load ox backends.

(use-package ox-odt
  :after org
  :defer t
  ;; FIXME
  ;; :custom
  ;; ((org-odt-data-dir (f-join user-emacs-directory "straight" "repos" "org" "etc")))
  )

(use-package ox-gfm
  :after org)

(use-package ox-slack
  :commands (org-slack-export-to-clipboard-as-slack))

(use-package ox-koma-letter
  :after org
  :defer t
  :config
  (add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil)))

(use-package ob-restclient
  :defer t)

;; General variables

(setq org-directory paths-org-directory)
(setq org-default-notes-file (f-join paths-org-directory "notes.org"))

(general-setq
 org-imenu-depth 4
 org-M-RET-may-split-line nil
 org-blank-before-new-entry '((heading . t) (plain-list-item . nil))
 org-catch-invisible-edits 'smart
 org-checkbox-hierarchical-statistics t
 org-confirm-elisp-link-function nil
 org-cycle-separator-lines 1
 org-enforce-todo-dependencies t
 org-footnote-auto-adjust t
 org-hide-emphasis-markers t
 org-hierarchical-todo-statistics nil
 org-indirect-buffer-display 'other-window
 org-insert-heading-respect-content t
 org-outline-path-complete-in-steps nil
 org-pretty-entities nil

 org-refile-targets '((org-funcs-refile-candidates . (:maxlevel . 3)))
 org-refile-allow-creating-parent-nodes 'confirm
 org-refile-target-verify-function #'org-funcs-refile-verify-function
 org-refile-use-outline-path 'file

 org-bookmark-names-plist nil

 org-return-follows-link t
 org-reverse-note-order nil
 org-startup-indented t
 org-startup-shrink-all-tables t
 org-startup-with-inline-images t
 org-adapt-indentation nil

 org-log-into-drawer t
 org-log-done 'time
 org-log-redeadline 'time
 org-log-reschedule 'time
 org-log-repeat 'time

 org-todo-keywords '((type "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c@)"))

 org-src-fontify-natively t
 org-src-window-setup 'current-window

 ;; I customise priorities so that headlines are either 'prioritised' an show a
 ;; cookie or 'cleared', which indicates no special priority.
 org-priority-start-cycle-with-default t
 org-highest-priority ?A
 org-lowest-priority ?B
 org-default-priority ?B


 ;; org-babel

 org-confirm-babel-evaluate nil
 org-babel-js-function-wrapper "console.log(require('util').inspect(function(){\n%s\n}()));"

 org-babel-load-languages '((emacs-lisp . t)
                            (calc . t)
                            (plantuml . t)
                            (ditaa . t)
                            (restclient . t)
                            (js . t)
                            (gnuplot . t)
                            (python . t)
                            (shell . t)
                            (sql . t))

 ;; org-export

 org-export-backends '(ascii html latex odt slack gfm koma-letter)
 org-html-html5-fancy t
 org-html-postamble nil
 org-export-exclude-tags '("noexport" "no_export" "crypt" "ignore")
 org-export-coding-system 'utf-8

 ;; latex

 org-latex-compiler "tectonic"
 org-latex-compilers '("tectonic" "pdflatex" "xelatex" "lualatex")
 org-latex-pdf-process '("tectonic --outdir %o %f")

 ;; org-habit

 org-habit-graph-column 68
 org-habit-preceding-days 7
 org-habit-following-days 5

 org-tags-exclude-from-inheritance '("crypt")
 org-crypt-disable-auto-save 'encypt

 org-attach-directory (f-join paths-org-directory "data")
 org-archive-location (concat (f-join org-directory "archive.org") "::datetree/")
 org-archive-subtree-add-inherited-tags t

 ;; appt

 appt-message-warning-time 60
 appt-display-interval 5

 ;; clocking

 org-clock-history-length 20
 org-clock-in-resume t
 org-clock-out-remove-zero-time-clocks t
 org-clock-persist t
 org-clock-into-drawer nil
 org-clock-persist-query-resume nil
 org-clock-report-include-clocking-task t

 ;; agenda
 org-stuck-projects '("" nil nil "")
 org-agenda-auto-exclude-function #'org-funcs-exclude-tasks-on-hold
 org-agenda-hide-tags-regexp (rx (or "noexport" "@someday"))
 org-agenda-include-diary nil
 org-agenda-insert-diary-extract-time t
 org-agenda-search-view-always-boolean t
 org-agenda-show-all-dates nil
 org-agenda-show-inherited-tags nil
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
 org-agenda-skip-scheduled-if-done t
 org-agenda-span 'day
 org-agenda-start-on-weekday nil
 org-agenda-window-setup 'only-window
 org-agenda-dim-blocked-tasks 'invisible
 org-agenda-sorting-strategy '((agenda time-up category-up priority-down todo-state-up)
                               (todo priority-down category-up scheduled-up)
                               (tags priority-down category-up)
                               (search category-up))
 org-agenda-inhibit-startup t
 org-agenda-tags-column -100
 org-agenda-text-search-extra-files (list (f-join org-directory "archive.org"))
 org-agenda-use-time-grid nil)

;; Use rg to search for candidates for org-agenda-files

(defun config-org--org-files-with-todos (dir)
  (with-temp-buffer
    (let ((default-directory dir))
      (call-process "rg" nil t nil
                    "TODO|CANCELLED|WAITING|DONE"
                    "--files-with-matches"
                    "--type" "org"
                    "--case-sensitive"
                    "--max-depth" "1")
      (let ((results (split-string (buffer-substring (point-min) (point-max)) "\n" t)))
        (seq-map (lambda (it) (f-join dir it))
                 results)))))

(ignore-errors
  (memoize #'config-org--org-files-with-todos "1 minute"))

(defun config-org--find-org-files-with-todos ()
  (let* ((dirs (-flatten (list org-directory
                               (when (bound-and-true-p org-roam-directory)
                                 (list org-roam-directory
                                       (f-join org-roam-directory "dailies")
                                       (f-join org-roam-directory "projects"))))))
         (files (seq-mapcat #'config-org--org-files-with-todos (seq-filter #'identity dirs))))
    (seq-filter (lambda (file)
                  ;; ignore dropbox conflict files
                  (not (s-contains-p "conflicted copy" (f-filename file))))
                files)))

(defun config-org-update-agenda-files ()
  (setq org-agenda-files (config-org--find-org-files-with-todos)))

(add-hook 'org-roam-mode-hook #'config-org-update-agenda-files)

;; Update agenda files as we create new org buffers.

(defun config-org--buffer-has-todo-keywords ()
  (and (derived-mode-p 'org-mode)
       (save-match-data
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp org-todo-regexp nil t)))))

(defun config-org-maybe-add-to-agenda-files ()
  (when (and (derived-mode-p 'org-mode)
             (config-org--buffer-has-todo-keywords)
             (buffer-file-name))
    (add-to-list 'org-agenda-files (buffer-file-name))))

(add-hook 'org-capture-after-finalize-hook #'config-org-maybe-add-to-agenda-files)



;; `org-funcs' provides supporting commands we want to bind.
(use-package org-funcs
  :init
  (setq initial-buffer-choice #'org-funcs-agenda-dwim)
  :general
  ("<f12>" #'org-funcs-punch-in-or-out)
  (:states 'normal :keymaps 'org-mode-map
   "C-c p" #'org-funcs-toggle-priority
   "C-c RET" #'org-funcs-ctrl-c-ret
   "C-c C-k" #'org-funcs-ctrl-c-ctrl-k)
  (:states 'motion :keymaps 'org-agenda-mode-map
   "C-c p" #'org-funcs-agenda-toggle-priority))

;; Configure the main `org' package.
(use-package org
  :defer t
  :commands (org-mode)
  :general
  ("C-c a" #'org-agenda
   "C-c s" #'org-search-view
   "C-c t" #'org-todo-list
   "C-c /" #'org-tags-view)
  (:states '(emacs normal) :keymaps 'org-mode-map
   "<backtab>" #'org-global-cycle
   "<tab>" #'org-cycle
   "C-c c" #'org-columns
   "M-n" #'org-metadown
   "M-p" #'org-metaup
   "RET" #'org-return)
  (:states '(normal motion insert emacs) :keymaps 'org-mode-map
   "C-c C-." #'org-time-stamp-inactive
   "C-c ." #'org-time-stamp)
  :preface
  (progn
    (defun config-org--exit-minibuffer (&rest _)
      "Exit minibuffer before adding notes."
      (when (minibufferp (window-buffer (selected-window)))
        (other-window 1)))

    (defun config-org--toggle-heading-goto-eol (&rest _)
      "Prevent point from moving to BOL when toggling headings."
      (when (s-matches? (rx bol (+ "*") (* space) eol)
                        (buffer-substring (line-beginning-position) (line-end-position)))
        (goto-char (line-end-position))))

    (defun config-org--set-local-vars-and-hooks ()
      (org-indent-mode +1)
      (add-hook 'after-save-hook #'config-org-maybe-add-to-agenda-files nil t)
      (setq bidi-paragraph-direction nil))

    (defun config-org--after-refile (&rest _)
      (org-save-all-org-buffers))

    (defun config-org--before-archive (&rest _)
      ;; Ensure we have a context before archiving.
      (unless (seq-intersection '("@personal" "@work" "@flat") (org-get-tags))
        (let ((tag (pcase-exhaustive (read-char-choice "Set context: [w]ork  [p]ersonal  [f]lat" '(?w ?p ?f))
                     (?w "@work")
                     (?p "@personal")
                     (?f "@flat"))))
          (org-toggle-tag tag 'on))))

    (defun config-org--after-archive (&rest _)
      (org-save-all-org-buffers))

    (defun config-org--before-agenda (&rest _)
      (config-org-update-agenda-files))

    ;; KLUDGE: Declare removed dynamic variable in case I revert to builtin org
    ;; version.
    (defvar org-log-states)

    (defun config-org--children-done-parent-done (_n-done n-todo)
      "Mark the parent task as done when all children are completed."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (zerop n-todo) "DONE" "TODO")))))

  :hook (org-mode . visual-line-mode)

  :commands
  (org-refile)

  :init
  (progn
    (advice-add 'org-agenda :before #'config-org--before-agenda)

    (advice-add 'org-refile :after #'config-org--after-refile)
    (advice-add 'org-archive-subtree :before #'config-org--before-archive)
    (advice-add 'org-archive-subtree :after #'config-org--after-archive)

    (add-hook 'org-mode-hook #'auto-revert-mode)
    (add-hook 'org-mode-hook #'config-org--set-local-vars-and-hooks)
    (add-hook 'org-after-todo-statistics-hook #'config-org--children-done-parent-done))

  :config
  (progn
    ;; Make bullets prettier.
    (font-lock-add-keywords 'org-mode
                    `((,(rx bol (* space) (group "-") (+ space))
                       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))
                      (,(rx bol (* space) (group "#+begin_src") symbol-end)
                       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "λ"))))
                      (,(rx bol (* space) (group "#+end_src") symbol-end)
                       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "⋱"))))
                      (,(rx bol (* space) (group "#+begin_quote") symbol-end)
                       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "“"))))
                      (,(rx bol (* space) (group "#+end_quote") symbol-end)
                       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "”"))))))

    ;; KLUDGE: org-babel only loads langs correctly through the custom variable
    ;; setter. It basically does the below.
    (dolist (lang (mapcar #'car org-babel-load-languages))
      (require (intern (format "ob-%s" lang))))

    (with-eval-after-load 'evil
      (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle))

    (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file-other-window)

    (add-to-list 'org-latex-default-packages-alist
                 '("colorlinks=true" "hyperref" nil))

    (org-clock-persistence-insinuate)
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    (advice-add 'org-add-log-note :before #'config-org--exit-minibuffer)
    (advice-add 'org-toggle-heading :after #'config-org--toggle-heading-goto-eol)))

;; `org-attach' provides file attachment functionality.
(use-package org-attach
  :defer t
  :after org
  :config
  (require 'org-attach-git))

;; `org-capture' provides functionality for quickly inserting templated content
;; into org files.
(use-package org-capture
  :defer t
  :after org
  :config
  (org-funcs-update-capture-templates
   (list
    (org-funcs-capture-template
     "t" "Todo" '(function org-funcs-dailies-file-for-capture) '(function org-funcs-capture-todo)
     :prepend nil)

    (org-funcs-capture-template
     "l" "Link" '(function org-funcs-dailies-file-for-capture) '(function org-funcs-capture-link)
     :prepend nil
     :immediate-finish t))))

;; `org-hydras' provides a few org-specific hydras.
(use-package org-hydras
  :commands (org-babel/body))

;; `org-agenda' provides mechanisms for collating and inspecting org headings
;; and other data sources into a single planning/agenda buffer.
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
   "B" #'org-agenda-bulk-action
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
   "C-b" #'evil-scroll-page-up

   ;; Restore bindings for search buffers
   "+" #'org-agenda-manipulate-query-add
   "`" #'org-agenda-manipulate-query-add-re
   "-" #'org-agenda-manipulate-query-subtract
   "_" #'org-agenda-manipulate-query-subtract-re)
  :preface
  (progn
    (autoload 'page-break-lines--update-display-tables "page-break-lines")

    (defconst config-org--agenda-clockreport-defaults
      '(:link t :compact t :maxlevel 4 :fileskip0 t :step week))

    (defun config-org--standard-filter-preset (tags)
      (seq-uniq (append tags '("-ignore"))))

    (cl-defun config-org--agenda-for-context (tag &key show-catchups-p filter-preset)
      `(,(concat (substring tag 1 2) "a")
        ,(format "Agenda for context: %s" tag)
        ,(-non-nil
          `((agenda ""
                    ((org-agenda-overriding-header "Today")
                     (org-agenda-use-time-grid t)))
            (tags-todo "-catchups&TODO=\"TODO\"|+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-skip-function #'org-funcs-skip-items-already-in-agenda)))

            ,(when show-catchups-p
               `(tags-todo "+catchups&-PRIORITY=\"A\"-TODO=\"WAITING\""
                           ((org-agenda-overriding-header "People & Catchup Topics")
                            (org-agenda-skip-function #'org-funcs-skip-item-if-timestamp))))

            (todo "WAITING"
                  ((org-agenda-overriding-header "Delegated")
                   (org-agenda-skip-function #'org-funcs-skip-item-if-timestamp)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Stuck Projects")
                   (org-agenda-skip-function #'org-project-skip-non-stuck-projects)))))

        ((org-agenda-tag-filter-preset ',(config-org--standard-filter-preset filter-preset))
         (org-agenda-start-with-log-mode '(closed clock state))
         (org-agenda-clockreport-parameter-plist ',(append config-org--agenda-clockreport-defaults (list :tags tag)))
         (org-agenda-span 'day)
         (org-agenda-show-future-repeats nil)
         (org-agenda-archives-mode nil)
         (org-agenda-ignore-drawer-properties '(effort appt)))))

    (cl-defun config-org--plan-for-context (tag-or-tags &key filter-preset)
      (let ((tags (-list tag-or-tags)))
        (cl-assert tags t "At least one tag must be supplied")
        `(,(concat (substring (car tags) 1 2) "p")
          ,(format "Plan for context: %s" (s-join ", " tags))
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
           (todo "TODO"
                 ((org-agenda-overriding-header "Review projects. Are these all healthy?")
                  (org-agenda-skip-function #'org-project-skip-non-projects))))
          ((org-agenda-tag-filter ',(config-org--standard-filter-preset filter-preset))
           (org-agenda-clockreport-parameter-plist ',(append config-org--agenda-clockreport-defaults (list :tags tags)))
           (org-agenda-span 'week)
           (org-agenda-show-future-repeats nil)
           (org-agenda-archives-mode nil)
           (org-agenda-ignore-drawer-properties '(effort appt))))))

    (cl-defun config-org--review-for-context (tag-or-tags &key filter-preset)
      (let ((tags (-list tag-or-tags)))
        (cl-assert tags t "At least one tag must be supplied")
        `(,(concat (substring (car tags) 1 2) "r")
          ,(format "Review for context: %s" (s-join ", " tags))
          ((agenda ""
                   ((org-agenda-overriding-header "Review agenda this week")
                    (org-agenda-use-time-grid nil)
                    (org-agenda-show-log t)))
           (todo "TODO"
                 ((org-agenda-overriding-header "Review Next Actions. Are these the next thing to do?")
                  (org-agenda-skip-function #'org-funcs-skip-items-already-in-agenda)))
           (tags-todo "+LEVEL=1+TODO=\"TODO\""
                      ((org-agenda-overriding-header "Review unscheduled actions. Any of these need to be prioritised?")
                       (org-agenda-skip-function #'org-funcs-skip-item-if-timestamp)))
           (todo "TODO"
                 ((org-agenda-overriding-header "Review projects. Are these all healthy?")
                  (org-agenda-skip-function #'org-project-skip-non-projects))))
          ((org-agenda-tag-filter-preset ',(config-org--standard-filter-preset filter-preset))
           (org-agenda-clockreport-parameter-plist ',(append config-org--agenda-clockreport-defaults (list :tags tags)))
           (org-agenda-span 'week)
           (org-agenda-start-day "-7d")
           (org-agenda-start-with-clockreport-mode t)
           (org-agenda-log-mode-items '(closed state))
           (org-agenda-show-future-repeats nil)
           (org-agenda-archives-mode nil)
           (org-agenda-ignore-drawer-properties '(effort appt))))))

    (defun config-org--draw-separator (&rest _)
      (page-break-lines--update-display-tables))

    (defun config-org--on-show-item-from-agenda ()
      (org-overview)
      (org-reveal)
      (org-show-subtree)
      (org-display-outline-path)))

  :init
  (with-eval-after-load 'org
    (require 'org-agenda))

  :config
  (progn
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
    (add-hook 'org-agenda-after-show-hook #'config-org--on-show-item-from-agenda)

    (org-funcs-update-agenda-custom-commands
     (list
      '("f" . "@flat context")
      (config-org--agenda-for-context "@flat"
                            :filter-preset '("+@flat" "-@someday"))
      '("p" . "@personal context")
      (config-org--agenda-for-context "@personal"
                            :filter-preset '("-@someday" "-@work"))
      (config-org--plan-for-context '("@personal" "@flat")
                          :filter-preset '("-@someday" "-@work"))

      (config-org--review-for-context '("@personal" "@flat"))

      '("w" . "@work context")
      (config-org--agenda-for-context "@work"
                            :filter-preset '("+@work" "-@someday")
                            :show-catchups-p t)
      (config-org--plan-for-context "@work" :filter-preset '("+@work" "-@someday"))
      (config-org--review-for-context "@work" :filter-preset '("+@work" "-@someday"))))

    ;; Ensure the separator line is rendered whenever the org agenda view
    ;; changes. This is needed for page-break-lines to render the separator
    ;; correctly.
    (setq org-agenda-block-separator "")
    (advice-add 'org-agenda :after #'config-org--draw-separator)
    (advice-add 'org-agenda-redo :after #'config-org--draw-separator)))

;; `org-archive' implements heading archival functionality.
(use-package org-archive
  :commands (org-archive-subtree)
  :preface
  (defun config-org--apply-inherited-tags (&rest _)
    "Apply inherited tags when archiving."
    (org-set-tags (org-get-tags)))

  :config
  (advice-add 'org-archive-subtree :before #'config-org--apply-inherited-tags))

;; `org-src' implements src code blocks.
(use-package org-src
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

;; `evil-org' provides better compatability with org-mode.
(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :general
  (:states 'normal :keymaps 'evil-org-mode-map "t" #'org-todo)
  :config
  ;; Remove weird keybindings.
  (general-unbind :states '(normal insert) :keymaps 'evil-org-mode-map
    "M-l" "M-h" "J" "O" "M-l" "M-h"))

;; `org-bullets' displays orgmode bullets using pretty utf-8 characters.
(use-package org-bullets
  :custom ((org-bullets-bullet-list '("○")))
  :hook (org-mode . org-bullets-mode))

;; Automatically enter insert state when inserting new headings or using
;; org-capture.
(with-eval-after-load 'evil

  (eval-and-compile
    (defun config-org--evil-insert-state-for-capture (&rest _)
      (when (and (called-interactively-p nil)
                 (bound-and-true-p org-capture-mode))
        (evil-insert-state)))

    (defun config-org--evil-insert-state (&rest _)
      (when (called-interactively-p nil)
        (evil-insert-state))))

  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state)
  (advice-add 'org-capture :after #'config-org--evil-insert-state-for-capture)
  (advice-add 'org-insert-heading :after #'config-org--evil-insert-state)
  (advice-add 'org-insert-heading-respect-content :after #'config-org--evil-insert-state)
  (advice-add 'org-insert-todo-heading-respect-content :after #'config-org--evil-insert-state)
  (advice-add 'org-insert-todo-heading :after #'config-org--evil-insert-state))

;; `cb-org-export-koma-letter' provides a C-c C-c handler for exporting the
;; heading at point as a koma letter.
(use-package cb-org-export-koma-letter
  :after org
  :commands (cb-org-export-koma-letter-handler)
  :config
  (progn
    (general-setq org-latex-hyperref-template "")
    (add-to-list 'org-latex-classes `("koma-letter" ,cb-org-export-koma-letter-latex-class))
    (add-hook 'org-ctrl-c-ctrl-c-hook #'cb-org-export-koma-letter-handler t)))

;; `htmlize' is required for HTML exports.
(use-package htmlize
  :defer t)

;; `org-id' provides support for linking to org headings via UUIDs.
(use-package org-id
  :after org
  :preface
  (progn
    (autoload 'org-id-get-create "org-id")
    (autoload 'org-id-store-link "org-id")

    ;; Transparently create ID properties when storing links to org headings. As
    ;; a corner case, avoid doing this as part of the capture process.

    (defvar config-org--capturing-p nil)

    (defun config-org--on-capture (f &rest args)
      (let ((config-org--capturing-p t)
            (org-link-parameters (--remove (equal "id" (car it)) org-link-parameters)))
        (apply f args)))

    (defun config-org--prompt-for-creating-id (f &rest args)
      (cond ((and (not config-org--capturing-p) (derived-mode-p 'org-mode) (org-at-heading-p))
             (let ((id (org-id-get-create))
                   (heading (org-link-display-format (substring-no-properties (org-get-heading t t t t)))))
               (org-id-store-link)
               (push (list (concat "id:" id) heading heading) org-stored-links)
               (message "Stored: %s" heading)))
            (t
             (apply f args)))))
  :init
  (progn
    (advice-add 'org-capture :around #'config-org--on-capture)
    (advice-add 'org-store-link :around #'config-org--prompt-for-creating-id))
  :config
  (org-link-set-parameters "id" :store #'org-id-store-link))

;; `org-edna' provides todo dependencies, triggers, and more complex repeaters.
(use-package org-edna
  :after org
  :preface
  (progn
    (defun config-org--at-edna-edit-context-p ()
      (save-excursion
        (let* ((element-type (ignore-errors
                               (org-element-type (org-element-context (org-element-at-point)))))
               (at-property-drawer-or-heading-p (seq-contains-p '(property-drawer node-property headline) element-type))
               (edna-loaded-p (seq-contains-p org-trigger-hook #'org-edna-trigger-function)))
          (and at-property-drawer-or-heading-p edna-loaded-p))))

    (defun config-org--maybe-edna-edit (fn &rest args)
      (if (config-org--at-edna-edit-context-p)
          (org-edna-edit)
        (apply fn args)))

    (defun config-org--revert-to-todo-if-edna-trigger-present ()
      (when-let ((trigger (org-entry-get (point) "TRIGGER")))
        (when (string-match-p (rx (or "scheduled!" "deadline!")) trigger)
          (let ((original-hook org-after-todo-state-change-hook))
            (remove-hook 'org-after-todo-state-change-hook #'config-org--revert-to-todo-if-edna-trigger-present)
            (unwind-protect (org-todo "TODO")
              (setq org-after-todo-state-change-hook original-hook)))))))
  :config
  (progn
    (org-edna-load)
    (advice-add 'org-edit-special :around #'config-org--maybe-edna-edit)

    ;; Ensure events with custom triggers don't get stuck in DONE because they don't have a

    (add-hook 'org-after-todo-state-change-hook #'config-org--revert-to-todo-if-edna-trigger-present)

    ;; Override org-enda-edit-finish so it doesn't break.

    (defun org-edna-edit-finish ()
      "Finish an Edna property edit."
      (interactive)
      ;; Remove properties from the values
      (let ((blocker (ignore-errors (substring-no-properties (org-edna-edit-blocker-section-text))))
            (trigger (ignore-errors (substring-no-properties (org-edna-edit-trigger-section-text))))
            (pos-marker org-edna-edit-original-marker)
            (wc org-window-configuration)
            (sel-win org-selected-window))
        (set-window-configuration wc)
        (select-window sel-win)
        (goto-char pos-marker)
        (when blocker
          (unless (string-empty-p blocker)
            (org-entry-put nil "BLOCKER" blocker)))
        (when blocker
          (unless (string-empty-p trigger)
            (org-entry-put nil "TRIGGER" trigger)))
        (kill-buffer org-edna-edit-buffer-name)))))

;; `org-present' implements presentations in org-mode.
(use-package org-present
  :commands (org-present)
  :preface
  (progn
    (defun config-org--maybe-next-slide (f &rest args)
      (if (bound-and-true-p org-present-mode)
          (org-present-next)
        (apply f args)))

    (defun config-org--maybe-previous-slide (f &rest args)
      (if (bound-and-true-p org-present-mode)
          (org-present-prev)
        (apply f args)))

    (defun config-org--on-start-presentation ()
      (org-present-big)
      (org-display-inline-images)
      (org-present-hide-cursor)
      (org-present-read-only)
      (hide-header-line-mode +1)
      (message "Starting presentation"))

    (defun config-org--on-end-presentation ()
      (org-present-small)
      (unless org-startup-with-inline-images
        (org-remove-inline-images))
      (org-present-show-cursor)
      (org-present-read-write)
      (hide-header-line-mode -1)
      (message "Exiting presentation")))
  :config
  (progn
    (setq org-present-text-scale 3)

    ;; KLUDGE: Need to override key bindings properly. This will do in the
    ;; meantime.
    (advice-add 'evil-search-next :around #'config-org--maybe-next-slide)
    (advice-add 'evil-search-previous :around #'config-org--maybe-previous-slide)
    (advice-add 'evil-paste-after :around #'config-org--maybe-previous-slide)

    (add-hook 'org-present-mode-hook #'config-org--on-start-presentation)
    (add-hook 'org-present-mode-quit-hook #'config-org--on-end-presentation)))

;; `org-protocol' provides a URL scheme and code triggering for Emacs.
(use-package org-protocol
  :after (:all server org)
  :config
  (org-funcs-update-capture-templates
   (list
    (org-funcs-capture-template " L" "org-protocol Link" '(file "inbox.org")
                                "%(org-funcs-read-url-for-capture \"%:link\" \"%:description\" t)\n%:initial"
                                :immediate-finish t))))

;; `om' provides a functional API for parsing and manipulating org files.
(use-package om
  :defer t)

;; `org-roam' provides a specialised orgmode workflow and backlinks.
(use-package org-roam
  :hook (after-init . org-roam-mode)
  :general
  (:states '(insert normal) :keymap 'org-mode-map
   "C-c i" 'org-roam-insert
   "C-c C-i" 'org-roam-insert
   "C-c I" 'org-roam-insert-immediate)
  :custom
  ((org-roam-directory (f-join paths-org-directory "roam"))
   (org-roam-db-location (f-join paths-cache-directory "org-roam.db"))))

;; `company-org-roam' provides a company backend for org-roam topics.
(use-package company-org-roam
  :after (:all org-roam company)
  :config
  (push 'company-org-roam company-backends))

;; `org-format-headings' provides some commands to clean up the whitespace
;; around org headings.
(use-package org-format-headings
  :commands (org-format-all-headings org-format-heading))

(provide 'config-org)

;;; config-org.el ends here
