;;; cb-org.el --- org-mode configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cb-macs)
(require 'f)

(cl-eval-when (compile)
  (require 'org))

(use-package org-funcs
  :autoload
  org-funcs-update-capture-templates
  :commands
  org-funcs-agenda-dwim
  org-funcs-todo-list
  org-funcs-read-url
  :general
  (:keymaps 'org-mode-map :states '(normal insert emacs)
   "C-c !" 'org-funcs-toggle-priority
   "C-c l" 'org-funcs-insert-url-as-link)
  (:keymaps 'org-agenda-mode-map :states 'motion
   "C-c !" 'org-funcs-agenda-toggle-priority))

(use-package org

  ;;; General settings

  :custom
  (org-directory "~/org")
  (org-bookmark-names-plist nil)
  (org-imenu-depth 4)
  (org-indirect-buffer-display 'current-window)
  (org-link-elisp-confirm-function 'y-or-n-p)
  (org-image-actual-width nil)
  (org-return-follows-link t)
  (org-use-property-inheritance t)
  (abbrev-file-name (expand-file-name "abbrev.el") org-directory)

  :config
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'abbrev-mode)

  ;;; Archive

  :custom
  (org-archive-tag "archived")
  (org-indent-mode-turns-on-hiding-stars t)
  (org-archive-location (concat (f-join org-directory "archive.org") "::datetree/"))
  (org-archive-subtree-add-inherited-tags nil)
  ;; Automatically save after archiving.
  :preface
  (define-advice org-archive-subtree (:after (&rest _) save-org-buffers)
    (org-save-all-org-buffers))

  ;;; Export

  :custom
  (org-export-backends '(ascii html latex odt slack gfm))
  (org-export-with-toc nil)
  (org-html-html5-fancy t)
  (org-html-postamble nil)
  (org-export-exclude-tags '("noexport" "ignore" "crypt"))
  (org-export-coding-system 'utf-8)

  ;;; Priorities

  :custom
  (org-priority-start-cycle-with-default t)
  (org-highest-priority ?A)
  (org-lowest-priority ?B)
  (org-default-priority ?B)

  ;;; Visual settings

  :custom
  (org-ellipsis " …")
  (org-pretty-entities t)
  (org-startup-indented t)
  (org-cycle-separator-lines 0)
  (org-hide-emphasis-markers t)
  (org-startup-with-latex-preview nil)
  (org-startup-folded 'showall)
  (org-startup-shrink-all-tables t)
  (org-startup-with-inline-images t)

  ;; Use bullet characeters for list items.
  :config
  (font-lock-add-keywords 'org-mode
                          `((,(rx bol (* space) (group "-") (+ space))
                             (0 (prog1 ()
                                  (compose-region (match-beginning 1)
                                                  (match-end 1)
                                                  "•"))))))
  ;;; Interactive behaviour

  :custom
  (org-M-RET-may-split-line nil)
  (org-adapt-indentation nil)
  (org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
  (org-catch-invisible-edits 'smart)
  (org-footnote-auto-adjust t)
  (org-insert-heading-respect-content t)
  (org-loop-over-headlines-in-active-region 'start-level)

  ;; Ensure we use dired rather than the Finder on macOS.
  :config
  (add-to-list 'org-file-apps '(directory . emacs))

  ;;; Attachments

  :custom
  (org-attach-auto-tag nil)
  (org-attach-id-dir "attach/")
  (org-attach-id-to-path-function-list '(identity))
  (org-attach-use-inheritance t)

  ;;; TODOs, checkboxes & stats

  :custom
  (org-todo-keywords '((type "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c@)")))
  (org-checkbox-hierarchical-statistics t)
  (org-checkbox-hierarchical-statistics t)
  (org-enforce-todo-dependencies t)
  (org-hierarchical-todo-statistics nil)

  ;; Completing all child TODOs will change the parent TODO to DONE.

  :preface
  (defun cb-org-maybe-complete-parent-todo (_n-done n-todo)
    (let (org-log-done) ; turn off logging
      (org-todo (if (zerop n-todo) "DONE" "TODO"))))
  :config
  (add-hook 'org-after-todo-statistics-hook
            #'cb-org-maybe-complete-parent-todo)

  ;; Don't show secondary selection when running `org-show-todo-tree'.
  :config
  (advice-add #'org-highlight-new-match :override #'ignore)

  ;;; Logging & clocking

  :custom
  (org-clock-history-length 20)
  (org-clock-in-resume t)
  (org-clock-into-drawer t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-persist t)
  (org-clock-persist-query-resume nil)
  (org-clock-report-include-clocking-task t)
  (org-clock-mode-line-total 'today)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-redeadline 'time)
  (org-log-repeat 'time)
  (org-log-reschedule 'time)
  (org-reverse-note-order nil)
  :config
  (org-clock-persistence-insinuate)

  ;; Exit minibuffer before adding notes.
  ;; :preface
  ;; (define-advice org-add-log-note (:before (&rest _) exit-minibuffer)
  ;;   (when (minibufferp (window-buffer (selected-window)))
  ;;     (other-window 1)))

  ;; Prevent erroneous spelling error highlights.
  :preface
  (define-advice org-mode-flyspell-verify (:filter-return (result))
    (and result (not (member (face-at-point nil t) '(org-formula org-link)))))

  ;; Capture

  ;; Automatically enter insert state when inserting new headings or using
  ;; `org-capture'.

  :preface
  (autoload 'evil-insert-state "evil-states")
  (define-advice org-capture (:after (&rest _) insert-state)
    (when (and (bound-and-true-p evil-mode)
               (called-interactively-p nil)
               (bound-and-true-p org-capture-mode))
      (evil-insert-state)))
  (defun cb-enter-evil-insert-state (&rest _)
    (when (and (bound-and-true-p evil-mode)
               (called-interactively-p nil))
      (evil-insert-state)))
  :init
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state)
  (advice-add 'org-insert-heading :after #'cb-enter-evil-insert-state)
  (advice-add 'org-insert-heading-respect-content :after #'cb-enter-evil-insert-state)
  (advice-add 'org-insert-todo-heading-respect-content :after #'cb-enter-evil-insert-state)
  (advice-add 'org-insert-todo-heading :after #'cb-enter-evil-insert-state)

  ;; Customise basic capture templates.
  ;;
  ;; More are added in my personal org config, so update rather than setting.

  :config
  (org-funcs-update-capture-templates
   '(("c" "Clocking")
     ("cn" "Note for currently clocked heading"
      plain
      (clock)
      (function org-funcs-capture-note-to-clocked-heading)
      :immediate-finish t)))

  ;;; Src blocks & babel

  :custom
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  (org-confirm-babel-evaluate nil)
  (org-babel-shell-names '("sh" "bash" "zsh" "oil"))
  (org-babel-load-languages '((emacs-lisp . t)
                              (C . t)
                              (clojure . t)
                              (csharp . t)
                              (dot . t)
                              (gnuplot . t)
                              (sql . t)
                              (python . t)
                              (http . t)
                              (calc . t)
                              (shell . t)))
  (org-babel-default-header-args:emacs-lisp '((:lexical . "yes")))
  (org-babel-clojure-backend 'cider)
  (org-babel-python-command "python3")

  ;; Prevent trailing whitespace from being created in src blocks.
  :preface
  (define-advice org-edit-src-exit (:before (&rest _) delete-ws)
    (delete-trailing-whitespace))
  (defun org-ad-suppress-final-newline ()
    (setq-local require-final-newline nil))
  :init
  (add-hook 'org-src-mode-hook 'org-ad-suppress-final-newline)

  ;; Show images in outputs (useful for GNUplot, etc).
  :config
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images t)

  ;;; Crypt
  :custom
  (org-tags-exclude-from-inheritance '("crypt"))
  (org-crypt-disable-auto-save 'encypt)
  :config
  (org-crypt-use-before-save-magic)

  ;;; Links

  ;; Prevent org-mode from following links in insert state
  :preface
  (autoload 'evil-insert-state-p "evil-states")
  (define-advice org-return (:around (fn &rest args) inhibit-follow-in-insert-state)
    (let ((org-return-follows-link (if (evil-insert-state-p)
                                       nil
                                     org-return-follows-link)))
      (apply fn args)))

  ;;; Link types

  :config
  (use-package ol-man :demand t)
  (use-package ol-mac-app :demand t)
  (use-package ol-linux-header :demand t)
  (use-package ol-ticker :demand t)
  (use-package ol-library :demand t)
  (use-package ol-dotnet-src
    :demand t
    :custom
    (ol-dotnet-src-runtime-location "~/src/dotnet/runtime"))

  ;;; LaTeX

  ;; Use Tectonic as org-mode's primary LaTeX compiler
  :custom
  (org-latex-compiler (getenv "NIX_EMACS_TEX_PROGRAM"))
  (org-latex-compilers (list (getenv "NIX_EMACS_TEX_PROGRAM")))
  (org-latex-pdf-process (list (concat (getenv "NIX_EMACS_TEX_PROGRAM") " -Z shell-escape --outdir=%o %f")))

  ;; Make latex previews look good on high-DPI screens
  :custom
  (org-latex-create-formula-image-program 'dvisvgm)
  :config
  (plist-put org-format-latex-options :scale 1.6)

  ;; Make C-c C-c toggle LaTeX fragment preview for editing
  :preface
  (defun cb-org-at-latex-fragment-p ()
    (let ((datum (org-element-context)))
      (and (memq (org-element-type datum) '(latex-environment latex-fragment)))))
  (defun cb-org-at-latex-preview-p ()
    (seq-find
     (lambda (ov)
       (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))
     (overlays-at (point))))
  (defun cb-org-maybe-toggle-latex-fragment ()
    (when (or (cb-org-at-latex-preview-p) (cb-org-at-latex-fragment-p))
      (org-latex-preview)))
  :init
  (add-hook 'org-ctrl-c-ctrl-c-hook #'cb-org-maybe-toggle-latex-fragment)

  ;; Default LaTeX packages
  :custom
  (org-highlight-latex-and-related '(native script entities))
  :config
  (add-to-list 'org-latex-default-packages-alist '("colorlinks=true" "hyperref" nil))

  ;; Don't apply org-block face to latex snippets.
  :config
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

  ;; LaTeX previews
  :custom
  (org-preview-latex-image-directory
   (no-littering-expand-var-file-name "org/latex-previews"))

  ;; Make previews use colour theme.
  :config
  (use-package org-latex-themed-previews
    :demand t
    :autoload org-latex-themed-previews-mode
    :config (org-latex-themed-previews-mode +1))

  ;;; Keybindings

  :general

  ("C-c a" 'org-agenda
   "C-c s" 'org-search-view
   "C-c f" 'org-footnote-new
   "C-c t" 'org-todo-list
   "C-c /" 'org-tags-view)

  (:states '(normal insert) :keymaps 'org-mode-map
   "C-c C-k"
   (general-predicate-dispatch 'org-cut-subtree
     (bound-and-true-p org-capture-mode) 'org-capture-kill
     (string-prefix-p "*Org" (buffer-name)) 'org-kill-note-or-show-branches)
   "C-c RET"
   (general-predicate-dispatch 'org-insert-todo-heading
     (org-at-table-p) 'org-table-hline-and-move))

  (:states '(insert) :keymaps 'org-mode-map
   "<tab>" (general-predicate-dispatch 'org-cycle
             (yas--templates-for-key-at-point) 'yas-expand))

  (:states '(emacs normal) :keymaps 'org-mode-map
   "<backtab>" 'org-global-cycle
   "<tab>" 'org-cycle
   "C-c c" 'org-columns
   "C-c d" 'org-dynamic-block-insert-dblock
   "C-c n" 'org-next-link
   "C-c p" 'org-previous-link
   "M-n" 'org-metadown
   "M-p" 'org-metaup
   "RET" 'org-open-at-point)

  (:states '(normal motion insert emacs) :keymaps 'org-mode-map
   "M-+" 'org-table-insert-column
   "M--" 'org-table-delete-column
   "C-c C-." 'org-time-stamp-inactive
   "C-c ." 'org-time-stamp
   "C-c o" 'org-table-toggle-coordinate-overlays))



(use-package org-agenda
  :custom
  (org-agenda-files (expand-file-name "org-agenda-files" org-directory))
  (org-stuck-projects '("" nil nil ""))
  (org-agenda-todo-ignore-scheduled t)
  (org-agenda-include-diary nil)
  (org-agenda-insert-diary-extract-time t)
  (org-agenda-search-view-always-boolean t)
  (org-agenda-show-all-dates nil)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday nil)
  (org-agenda-window-setup 'only-window)
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-agenda-sorting-strategy '((agenda time-up category-up priority-down todo-state-up)
                                 (todo priority-down category-up scheduled-up)
                                 (tags priority-down category-up)
                                 (search category-up)))
  (org-agenda-clock-report-header "\nClocking")
  (org-agenda-tags-column -100)
  (org-agenda-text-search-extra-files (list (expand-file-name "archive.org" org-directory)))
  (org-agenda-use-time-grid nil)

  ;; Automatically remove deleted files from agenda
  :preface
  (define-advice org-check-agenda-file (:override (file) always-remove-missing)
    (unless (file-exists-p file)
      (org-remove-file file)
      (throw 'nextfile t)))

  :general
  (:keymaps 'org-agenda-mode-map :states 'motion
   "/" 'org-agenda-filter
   "?" 'org-agenda-filter-by-tag
   "B" 'org-agenda-bulk-action
   "v" 'org-agenda-view-mode-dispatch
   "t" 'org-agenda-todo
   "J" 'org-agenda-goto-date
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   "f" 'org-agenda-later
   "b" 'org-agenda-earlier
   "M-j" 'org-agenda-next-item
   "M-k" 'org-agenda-previous-item
   "M-h" 'org-agenda-earlier
   "M-l" 'org-agenda-later
   "gd" 'org-agenda-toggle-time-grid
   "gr" 'org-agenda-redo
   "M-RET" 'org-agenda-show-and-scroll-up
   "C-f" 'evil-scroll-page-down
   "C-b" 'evil-scroll-page-up
   [remap save-buffer] 'org-save-all-org-buffers
   ;; Restore bindings for search buffers
   "+" 'org-agenda-manipulate-query-add
   "`" 'org-agenda-manipulate-query-add-re
   "-" 'org-agenda-manipulate-query-subtract
   "_" 'org-agenda-manipulate-query-subtract-re)


  ;;; Define agenda views

  :custom
  (org-agenda-start-with-log-mode '(closed state))
  (org-agenda-span 'day)
  (org-agenda-show-future-repeats nil)
  (org-agenda-ignore-properties '(effort appt))
  (org-agenda-clock-consistency-checks '(:gap-ok-around ("12:20" "12:40" "4:00")
                                         :max-duration "10:00"
                                         :min-duration 0
                                         :max-gap 0))

  ;; Define an agenda view that changes depending on whether I'm clocked in or not
  ;; (by invoking `org-funcs-agenda-dwim'). The presence of a running clock means I'm
  ;; in a work context.

  ;; The todos shown in the agenda are aggressively filtered using skip functions, so
  ;; that subtasks are preferred. This is used to model GTD-style /next actions/.
  ;; Filtering can be suppressed for a specific tree by setting the `AGENDA_SKIP'
  ;; property to `ignore' or `scheduled'.

  :custom
  (org-agenda-custom-commands
   (let ((todos '(tags-todo "-bills-project-outline+TODO=\"TODO\""
                            ((org-agenda-overriding-header "Next Actions")
                             (org-agenda-skip-function #'org-funcs-skip-items-already-in-agenda))))
         (bills '(tags-todo "+TODO=\"TODO\"+bills"
                            ((org-agenda-overriding-header "Bills")
                             (org-agenda-skip-function #'org-funcs-skip-items-already-in-agenda))))
         (projects '(tags-todo "+TODO=\"TODO\"+project"
                               ((org-agenda-overriding-header "Projects"))))
         (delegated '(todo "WAIT"
                           ((org-agenda-overriding-header "Delegated")
                            (org-agenda-skip-function #'org-funcs-skip-item-if-timestamp))))
         (today '(agenda ""
                         ((org-agenda-overriding-header "Today")
                          (org-agenda-clockreport-parameter-plist '(:compact t
                                                                    :link t
                                                                    :maxlevel 3
                                                                    :fileskip0 t
                                                                    :filetitle t))
                          (org-agenda-use-time-grid t))))
         (notes
          '(tags-todo "-bills+outline-project+TODO=\"TODO\""
                      ((org-agenda-overriding-header "Unprocessed Notes")
                       (org-agenda-skip-function #'org-funcs-skip-items-already-in-agenda)))))
     `(("p" "personal agenda" ,(list todos bills delegated today)
        ((org-agenda-tag-filter-preset '("-someday" "-ignore" "-work" "-outline"))
         (org-agenda-archives-mode t)))
       ("w" "work agenda" ,(list todos delegated projects today notes)
        ((org-agenda-clockreport-mode t)
         (org-agenda-tag-filter-preset (list "-someday" "-ignore" (format "+%s" (timekeep-work-tag))))
         (org-agenda-archives-mode t))))))

  ;; Use page-break-lines to draw separator
  :custom
  (org-agenda-block-separator (char-to-string ?\f))
  :preface
  (autoload 'page-break-lines--update-display-tables "page-break-lines")
  (define-advice org-agenda (:after (&rest _) draw-separator)
    (page-break-lines--update-display-tables))
  (define-advice org-agenda-redo (:after (&rest _) draw-separator)
    (page-break-lines--update-display-tables))

  ;; Reveal context around item on TAB
  :config
  (add-hook 'org-agenda-after-show-hook
            (lambda ()
              (org-overview)
              (org-reveal)
              (org-fold-show-subtree)
              (org-display-outline-path)))


  ;; Make priority inherited
  :preface
  (defun cb-org-inherited-priority (s)
    (save-match-data
      (cond
       ((string-match org-priority-regexp s)
        (* 1000 (- org-priority-lowest
                   (org-priority-to-value (match-string 2 s)))))
       ((not (org-up-heading-safe))
        (* 1000 (- org-priority-lowest org-priority-default)))
       (t
        (cb-org-inherited-priority (org-get-heading))))))
  :custom
  (org-priority-get-priority-function #'cb-org-inherited-priority))

;; Search for and update agenda files automatically

(defvar cb-org-agenda-update-process nil)

(defun cb-update-agenda-files-by-rg ()
  (unless (and cb-org-agenda-update-process (process-live-p cb-org-agenda-update-process))
    (let ((default-directory org-directory))
      (setq cb-org-agenda-update-process
            (start-process-shell-command "update-org-agenda-files" nil "rg --follow --files-with-matches '^(CLOCK:|[*]+ +(TODO|WAIT))' roam -g '!attach' -g '!dailies' > org-agenda-files")))))

(defun cb-setup-org-agenda-file-updates ()
  (when (derived-mode-p 'org-mode)
    (cb-update-agenda-files-by-rg)))

(add-hook 'after-save-hook #'cb-setup-org-agenda-file-updates)



(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package orgtbl-aggregate
  :after (org)
  :demand t)

(use-package ox-koma-letter
  :demand t
  :after ox)

(use-package org-download
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-method 'attach)
  (org-download-image-org-width 650)
  :config
  ;; Prevent insertion of download timestamp into buffer.
  (setq org-download-annotate-function (-const ""))

  ;; HACK: Fix throw in org-attach.
  :preface
  (define-advice org-download-image (:around (fn link))
    "Save image at address LINK to `org-download--dir'."
    (interactive "sUrl: ")
    (cond
     ((equal org-download-method 'attach)
      (let* ((link-and-ext (org-download--parse-link link))
             (filename
              (cond ((eq major-mode 'org-mode)
                     (let ((org-download-image-dir (org-attach-dir t))
                           org-download-heading-lvl)
                       (apply #'org-download--fullname link-and-ext)))
                    ((fboundp org-download-method)
                     (funcall org-download-method link))
                    (t
                     (apply #'org-download--fullname link-and-ext)))))
        (setq org-download-path-last-file filename)
        (org-download--image link filename)
        (when (derived-mode-p 'org-mode)
          (ignore-errors
            (org-attach-attach filename nil 'none))
          (org-download-insert-link link filename))))
     (t
      (funcall fn link)))))

(use-package org-download
  :if (equal system-type 'darwin)
  :custom
  (org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s")
  :general
  (:keymaps 'org-mode-map "s-V" 'org-download-screenshot))

;; Automatically reveal hidden org elements at point. This makes it much easier
;; to edit text using emphasis markers correctly.

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autokeywords t)
  (org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  :autoload (org-appear--set-elements)
  :config
  (run-at-time nil nil #'org-appear--set-elements))



(use-package org-format
  :hook (org-mode . org-format-on-save-mode))

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :custom
  (evil-org-key-theme '(additional
                        return
                        calendar
                        navigation
                        textobjects
                        todo)))

(use-package org-superstar
  :preface
  (defun cb-set-superstar-mode ()
    (org-superstar-mode (if (bound-and-true-p org-indent-mode) +1 -1)))
  :hook
  (org-indent-mode . cb-set-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '(?*))
  (org-superstar-leading-bullet ?\s)
  (org-superstar-special-todo-items t)
  (org-superstar-todo-bullet-alist '(("TODO" . #x25cb) ; ○
                                     ("WAIT" . #x25cb) ; ○
                                     ("CANCELLED" . #x25cf) ; ●
                                     ("DONE" . #x25cf))) ; ●
  :config
  (setf (alist-get 45 org-superstar-item-bullet-alist) #x2022)) ; •

(use-package org-attach-git
  :demand t
  :after org-attach)

(use-package ox-gfm
  :after org)

(use-package ox-slack
  :after org
  :preface
  (autoload 'slack-notes-translate-names "slack-notes")
  :custom
  (ox-slack-postprocess-function #'slack-notes-translate-names))

(use-package htmlize
  :defer t)



;;; Keys

;; https://stackoverflow.com/a/28031539
(defun cb-org-focus-entry ()
  "Show only the entry at point, keeping other entries closed."
  (interactive)
  (cond ((save-excursion (end-of-line) (outline-invisible-p))
         (progn
           (org-fold-show-entry)
           (org-fold-show-children)))
        (t
         (org-back-to-heading)
         (unless (and (bolp) (org-at-heading-p))
           (org-up-heading-safe)
           (org-fold-hide-subtree)
           (user-error "Boundary reached"))
         (org-overview)
         (org-reveal t)
         (org-fold-show-entry)
         (org-fold-show-children))))

(mode-leader-set-key :keymaps 'org-mode-map
  "A" '(org-archive-subtree :wk "archive")
  "/" '(org-sparse-tree :wk "sparse tree...")
  "#" '(org-decrypt-entries :wk "decrypt entries")
  "RET" '(org-indent-mode :wk "toggle indentation")
  "R" (list (general-predicate-dispatch 'org-roam-slipbox-refile
              (bound-and-true-p org-capture-mode) 'org-capture-refile)
            :wk "refile")
  "f" '(cb-org-focus-entry :wk "focus entry")

  "u" '(nil :wk "org-roam-ui")
  "u f" '(org-roam-ui-follow-mode :wk "toggle follow")
  "u o" '(org-roam-ui-open :wk "open in browser")
  "u z" '(org-roam-ui-node-zoom :wk "zoom")
  "u l" '(org-roam-ui-node-local :wk "local")

  "c" '(org-clock-display :wk "clock overlays")
  "g" '(org-id-get-create :wk "get/create heading ID")

  "x" '(org-cut-subtree :wk "cut")
  "y" '(org-copy-subtree :wk "copy")
  "p" '(org-paste-subtree :wk "past")
  "n" '(org-num-mode :wk "show heading numbers")
  "o" '(org-tree-to-indirect-buffer :wk "tree to indirect buf")
  "t" '(org-show-todo-tree :wk "todo tree")

  "e" '(nil :wk "babel")
  "e b" '(org-babel-execute-buffer :wk "execute buffer")
  "e c" '(org-babel-tangle-clean :wk "clean")

  "e e" (list (general-predicate-dispatch 'org-babel-execute-subtree
                (org-in-src-block-p) 'org-babel-execute-src-block)
              :wk "execute")
  "e i" '(org-babel-view-src-block-info :wk "src info")

  "e x" '(org-babel-demarcate-block :wk "split block")
  "e v" '(org-babel-mark-block :wk "mark block"))



(provide 'cb-org)

;;; cb-org.el ends here
