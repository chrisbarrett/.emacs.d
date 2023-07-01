;;; cb-leader.el --- Leader key support.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cl-lib)
(require 's)

(cl-eval-when (compile)
  (require 'general))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-sort-uppercase-first nil)
  (which-key-idle-delay 0.4))

(defmacro leader-set-key (&rest args)
  (declare (indent defun))
  `(use-package general
     :after evil
     :demand t
     :config
     (,'general-def ,@args ,@'(:keymaps 'override :states
                               '(normal motion visual)
                               :prefix "SPC"))))

(general-unbind :keymaps 'magit-section-mode-map "SPC")
(general-unbind :states '(normal motion) "SPC")



;;; Top-level

(defun cb-alternate-buffer (&optional window)
  "Toggle back and forth between two buffers in WINDOW.

WINDOW sets the window in which to toggle, and defaults to the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (car (seq-filter (lambda (buffer)
                            (and (not (eq buffer current-buffer))
                                 (or (null buffer-predicate) (funcall buffer-predicate buffer))))
                          (seq-map #'car (window-prev-buffers window))))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(leader-set-key
  "-" '(window-toggle-side-windows :wk "toggle side windows")
  "!" '(async-shell-command :wk "shell cmd (async)")
  "'" (general-predicate-dispatch 'poporg-dwim
        (bound-and-true-p poporg-mode) 'poporg-edit-exit
        (bound-and-true-p edit-indirect--overlay) 'edit-indirect-commit
        (equal (buffer-name) "*Edit Formulas*") 'org-table-fedit-finish
        (derived-mode-p 'org-mode) 'org-edit-special
        (and (derived-mode-p 'markdown-mode) (markdown-code-block-at-point-p)) 'markdown-edit-code-block
        (bound-and-true-p org-src-mode) 'org-edit-src-exit)
  "/" (list (general-predicate-dispatch 'consult-ripgrep
              (or (f-descendant-of-p default-directory org-directory)
                  (f-same-p default-directory org-directory))
              'org-roam-consult)
            :wk "search...")
  ":" '(better-eval-expression :wk "eval")
  ";" '(ielm :wk "Lisp REPL")
  "<tab>" '(cb-alternate-buffer :wk "other buf")
  "?" '(general-describe-keybindings :wk "show bindings")
  "@" '(consult-bookmark :wk "bookmark")
  "|" '(rotate-layout :wk "rotate window layout")
  "SPC" '(consult-buffer :wk "switch buf")
  "C" #'compile
  "D" '(dired-other-window :wk "dired (other)")
  "S" '(deadgrep :wk "rg (deadgrep)")
  "d" '(dirvish-dwim :wk "dired")
  "i" '(consult-imenu :wk "imenu")
  "q" '(delete-window :wk "delete window")
  "s" '(evil-iedit-state/iedit-mode :wk "iedit")
  "u" '(universal-argument :wk "prefix arg")
  "x" '(execute-extended-command :wk "M-x"))



;;; , - Parens

(leader-set-key :infix ","
  "" '(nil :wk "parens")
  "h" '(sp-beginning-of-sexp :wk "go to start")
  "l" '(sp-end-of-sexp :wk "go to end")
  "n" '(sp-next-sexp :wk "next")
  "p" '(sp-previous-sexp :wk "prev")
  "<" '(sp-backward-up-sexp :wk "backward up")
  ">" '(sp-up-sexp :wk "up")
  "c" '(sp-convolute-sexp :wk "convolute")
  "d" '(sp-kill-sexp :wk "kill")
  "D" '(sp-backward-kill-sexp :wk "kill backward")
  "k" '(sp-splice-sexp-killing-forward :wk "splice (forward)")
  "K" '(sp-splice-sexp-killing-backward :wk "splice (back)")
  "s" '(sp-splice-sexp-killing-around :wk "splice (around)")
  "r" '(sp-raise-sexp :wk "raise")
  "a" '(sp-add-to-next-sexp :wk "add to next")
  "A" '(sp-add-to-previous-sexp :wk "add to prev")
  "b" '(sp-forward-barf-sexp :wk "barf (forward)")
  "B" '(sp-backward-barf-sexp :wk "barf (back)")
  "m" '(sp-forward-slurp-sexp :wk "slurp (forward)")
  "M" '(sp-backward-slurp-sexp :wk "slurp (back)")
  "e" '(sp-emit-sexp :wk "emit")
  "j" '(sp-join-sexp :wk "joi")
  "t" '(sp-transpose-sexp :wk "transpose")
  "U" '(sp-backward-unwrap-sexp :wk "unwrap (back)")
  "u" '(sp-unwrap-sexp :wk "unwrap (forward)")
  "w" '(sp-rewrap-sexp :wk "rewrap")
  "x" '(sp-split-sexp :wk "split")
  "y" '(sp-copy-sexp :wk "copy (forward)")
  "Y" '(sp-backward-copy-sexp :wk "copy (back)"))



;;; a - Applications

(use-package profiler
  :autoload (profiler-running-p profiler-stop profiler-report)
  :preface
  (defun cb-profiler-stop-and-report (&optional continue-p)
    "Stop the profiler and show results.

With optional prefix arg CONTINUE-P, keep profiling."
    (interactive "P")
    (let ((ran-p (profiler-running-p)))

      (unless continue-p
        (profiler-stop))
      (profiler-report)
      (when ran-p
        (if continue-p
            (message "Profiler still recording")
          (message "Profiler stopped"))))))

(leader-set-key :infix "a"
  "" '(nil :wk "apps")
  "c" #'quick-calc
  "C" #'full-calc
  "e" #'eshell
  "g" '(chatgpt-shell :wk "ChatGPT")
  "p" '(dall-e-shell :wk "DALL-E")

  "d" '(nil :wk "debugger")
  "dd" '(edebug-where :wk "return to session")
  "dq" '(edebug-top-level-nonstop :wk "toplevel")
  "ds" '(edebug-stop :wk "stop")
  "d?" '(edebug-help :wk "help")
  "dl" '(edebug-visit-eval-list :wk "switch to eval list")
  "dx" '(edebug-remove-instrumentation :wk "unset")

  "r" (general-predicate-dispatch 'profiler-start
        (and (featurep 'profiler) (profiler-running-p)) 'cb-profiler-stop-and-report))



;;; b - Buffers

(leader-set-key :infix "b"
  "" '(nil :wk "bufs")
  "n" '(next-buffer :wk "next")
  "p" '(previous-buffer :wk "prev")
  "l" '(bufler :wk "list")
  "s" '(consult-buffer :wk "switch...")
  "S" '(consult-buffer-other-window :wk "switch... (other window)")
  "b" '(bury-buffer :wk "bury")
  "d" '(kill-current-buffer :wk "kill")
  "w" '(save-buffer :wk "save"))



;;; c - Commenting

(leader-set-key :infix "c"
  "" '(nil :wk "comments")
  "l" '(evilnc-comment-or-uncomment-lines :wk "line")
  "r" '(comment-or-uncomment-region :wk "region"))



;;; e - LSP & errors

(cl-eval-when (compile)
  (require 'flymake))

(defun cb-flymake-toggle-buffer-error-list ()
  "Show or hide the buffer error list."
  (interactive)
  (if-let* ((window (seq-find (lambda (it)
                                (equal (flymake--diagnostics-buffer-name)
                                       (buffer-name (window-buffer it))))
                              (window-list))))
      (delete-window window)
    (flymake-show-buffer-diagnostics)))

(defun cb-flymake-toggle-project-error-list ()
  "Show or hide the project error list."
  (interactive)
  (if-let* ((window (seq-find (lambda (it)
                                (equal (format "*Flymake diagnostics for `%s'*" (project-root (project-current)))
                                       (buffer-name (window-buffer it))))
                              (window-list))))
      (delete-window window)
    (flymake-show-project-diagnostics)))

(leader-set-key :infix "e"
  "" '(nil :wk "LSP & errors")
  "n" '(flymake-goto-next-error :wk "next")
  "p" '(flymake-goto-prev-error :wk "prev")
  "l" '(cb-flymake-toggle-buffer-error-list :wk "list (buffer)")
  "L" '(cb-flymake-toggle-project-error-list :wk "list (project)")
  "r" '(eglot-rename :wk "rename...")
  "f" '(eglot-format :wk "format")

  "b" '(nil :wk "backends...")
  "br" '(flymake-reporting-backends :wk "reporting")
  "bb" '(flymake-running-backends :wk "running")
  "bd" '(flymake-disabled-backends :wk "disabled"))



;;; f - Files

(defun cb-delete-current-buffer-and-file ()
  "Remove the file associated with the current buffer, then kill it."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((null file)
      (kill-buffer))
     ((not (file-exists-p file))
      (kill-buffer))
     ((yes-or-no-p "Delete this file? ")
      (delete-file file t)
      (kill-buffer)
      (message "File deleted: %s" file)))))

(defun cb-sudo-edit (&optional arg)
  "Reopen the current file as sudo for editing.

With prefix argument ARG, prompt for a file."
  (interactive "p")
  (let* ((fname (if (or arg (not buffer-file-name))
                    (read-file-name "File: ")
                  buffer-file-name))
         (target (cond ((string-match-p "^/ssh:" fname)
                        (with-temp-buffer
                          (insert fname)
                          (search-backward ":")
                          (let ((last-match-end nil)
                                (last-ssh-hostname nil))
                            (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                              (setq last-ssh-hostname (or (match-string 1 fname)
                                                          last-ssh-hostname))
                              (setq last-match-end (match-end 0)))
                            (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
                          (buffer-string)))
                       (t (concat "/sudo:root@localhost:" fname)))))
    (find-file target)))

(defun cb-reload-file ()
  "Revisit the current file."
  (interactive)
  (when-let* ((path (buffer-file-name)))
    (find-alternate-file path)))

(defun cb-copy-buffer-path ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (if-let* ((path (or (buffer-file-name) list-buffers-directory)))
      (progn
        (kill-new path)
        (message "%s" path))
    (error "Buffer not visiting a file")))

(defun cb-copy-buffer-name ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  (let ((name (if-let* ((path (buffer-file-name)))
                  (file-name-nondirectory path)
                (buffer-name))))
    (kill-new name)
    (message "%s" name)))

(defun cb-copy-buffer-directory ()
  "Show and copy the directory of the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (if-let* ((path (or (ignore-errors (file-name-directory (buffer-file-name))) list-buffers-directory)))
      (progn
        (kill-new path)
        (message "%s" path))
    (error "Buffer not visiting a file")))

(leader-set-key :infix "f"
  "" '(nil :wk "files")
  "d" '(cb-copy-buffer-directory :wk "copy dir")
  "y" '(cb-copy-buffer-path :wk "copy path")
  "Y" '(cb-copy-buffer-name :wk "copy name")
  "D" '(cb-delete-current-buffer-and-file :wk "delete buf & file")
  "e" 'cb-sudo-edit
  "f" '(find-file :wk "find...")
  "F" '(find-file-other-window :wk "find... (other window)")
  "s" '(save-buffer :wk "save")
  "S" '(save-some-buffers :wk "save... (interactive)")
  "l" '(find-file-literally :wk "find literally...")
  "l" '(hexl-find-file :wk "find as hex...")
  "w" '(write-file :wk "write copy...")
  "v" '(cb-reload-file :wk "reload from disk")
  "r" '(consult-recent-file :wk "recent files...")
  "R" '(magit-file-rename :wk "rename..."))



;;; g - Git & goto

(require 'xref)

(defcustom cb-nix-directory "~/.config/nixpkgs/"
  "Path to Nix configuration files."
  :group 'cb-leader
  :type 'directory)

(defun cb-jump-to-file (file &optional pos)
  (xref-push-marker-stack)
  (let ((buf (or (get-buffer file) (find-file-noselect file))))
    (switch-to-buffer buf)
    (when pos
      (goto-char pos))))

(defun cb-jump-to-config-file ()
  "Jump to the config.org file."
  (interactive)
  (cb-jump-to-file (expand-file-name "config.org" user-emacs-directory)))

(defun cb-jump-to-tangled-config-file ()
  "Jump to the config.el file."
  (interactive)
  (cb-jump-to-file (expand-file-name "config.el" user-emacs-directory)))

(defun cb-jump-to-emacs-flake ()
  "Jump to the packages.nix file."
  (interactive)
  (cb-jump-to-file (expand-file-name "flake.nix" user-emacs-directory)))

(defun cb-jump-to-init-file ()
  "Open the Emacs init.el file."
  (interactive)
  (cb-jump-to-file (expand-file-name "init.el" user-emacs-directory)))

(defun cb-jump-to-nix-config ()
  "Open a nix config file."
  (interactive)
  (let ((default-directory cb-nix-directory))
    (project-find-file)))

(defun cb-jump-to-nix-system-config ()
  "Open the nix system config file."
  (interactive)
  (save-match-data
    (let* ((default-directory cb-nix-directory)
           (hostname
            (cadr (s-match (rx (group (+? nonl)) (? "-" (+ digit)) (? ".local") eos)
                           (downcase (system-name)))))
           (prefix (format "hosts/%s.%s" hostname system-type))
           (default-nix (format "hosts/%s.%s/default.nix" hostname system-type)))

      (cb-jump-to-file (if (file-exists-p default-nix)
                           default-nix
                         (concat prefix ".nix"))))))

(defun cb-jump-to-site-file ()
  "Open the Emacs site config file."
  (interactive)
  (cb-jump-to-file (expand-file-name "config/default.el" user-emacs-directory)))

(defun cb-jump-to-messages ()
  "Open the messages buffer."
  (interactive)
  (display-buffer "*Messages*"))

(leader-set-key :infix "g"
  "" '(nil :wk "git/goto")
  "c" '(cb-jump-to-config-file :wk "to config.org")
  "C" '(cb-jump-to-tangled-config-file :wk "to tangled config")
  "f" '(cb-jump-to-emacs-flake :wk "to Emacs' flake.nix")
  "i" '(cb-jump-to-init-file :wk "to init file")
  "n" '(cb-jump-to-nix-config :wk "to Nix config")
  "S" '(cb-jump-to-nix-system-config :wk "to system Nix config")
  "?" '(cb-jump-to-messages :wk "to messages buf")
  "S" '(cb-jump-to-site-file :wk "to site.el")
  "s" '(magit-status :wk "magit")
  "d" '(magit-diff-buffer-file :wk "git diff of file")
  "b" '(magit-blame :wk "git blame")
  "r" '(browse-at-remote :wk "git remote: browse")
  "y" '(browse-at-remote-kill :wk " git remote: copy")
  "l" '(magit-log-buffer-file :wk "git log")
  "w" '(magit-worktree-status :wk "git worktree...")
  "W" '(magit-worktree :wk "git worktree popup...")
  "g" '(xref-find-definitions :wk "find defs")
  "G" '(xref-find-definitions-other-window :wk "find def (other window)")
  "m" '(xref-find-references :wk "find references")
  "SPC" 'pop-tag-mark)



;;; h - Help

(leader-set-key :infix "h"
  "" '(nil :wk "help")
  "i" #'info
  "m" #'man
  "d" '(nil :wk "describe")
  "d c" '(describe-face :wk "face...")
  "d C" '(helpful-command :wk "command...")
  "d f" '(helpful-callable :wk "function...")
  "d k" '(helpful-key :wk "key...")
  "d m" '(describe-mode :wk "mode")
  "d p" '(describe-text-properties :wk "properties at pt")
  "d v" '(helpful-variable :wk "variable...")
  "f" '(nil :wk "find")
  "f c" '(find-face-definition :wk "face...")
  "f f" '(find-function :wk "function...")
  "f l" '(find-library :wk "lisp library...")
  "f v" '(find-variable :wk "variable..."))



;;; k - Killing

(leader-set-key :infix "k"
  "" '(nil :wk "kill")
  "b" 'kill-this-buffer
  "w" 'delete-window
  "r" 'consult-yank-pop)



;;; n - Narrowing

(leader-set-key :infix "n"
  "" '(nil :wk "narrow")
  "e" '(edit-indirect-region :wk "edit (indirect)")
  "f" '(narrow-to-defun :wk "defun")
  "r" '(narrow-to-region :wk "region")
  "w" 'widen
  "s" '(org-narrow-to-subtree :wk "subtree")
  "S" '(org-tree-to-indirect-buffer :wk "tree to indirect buffer"))



;;; o - org-mode

(cl-eval-when (compile)
  (require 'org-roam))

(defcustom org-roam-index-node-id nil
  "ID for the main slipbox index."
  :group 'cb-leader
  :type 'string)

(defun cb-jump-to-index-file ()
  "Jump to the slipbox index file."
  (interactive)
  (org-roam-node-visit (org-roam-node-from-id org-roam-index-node-id)))

(defun cb-jump-to-notes-file ()
  "Jump to the scratch org file."
  (interactive)
  (find-file (expand-file-name "notes.org" org-directory)))

(defun cb-jump-to-org-init ()
  "Jump to the Lisp configuration in `org-directory'."
  (interactive)
  (find-file (expand-file-name "lisp/init.el" org-directory)))

(defun cb-org-goto-accounts ()
  "Goto the accounts file."
  (interactive)
  (org-roam-node-visit (org-roam-node-from-title-or-alias "Accounts")))

(leader-set-key :infix "o"
  "" '(nil :wk "org")
  "$" '(cb-org-goto-accounts :wk "accounts")
  "/" '(org-ql-search :wk "search...")
  "a" '(org-funcs-agenda-dwim :wk "agenda")

  "n" '(cb-jump-to-notes-file :wk "goto notes")

  "c" '(nil :wk "clock")
  "c i" '(timekeep-start :wk "punch in")
  "c o" '(timekeep-stop :wk "punch out")
  "c r" '(org-resolve-clocks :wk "resolve clocks")
  "c g" '(org-clock-goto :wk "goto last clock")

  "i" '(cb-jump-to-index-file :wk "index file")
  "j" '(cb-jump-to-org-init :wk "lisp init file")

  "k" '(org-capture :wk "capture...")
  "K" '(org-roam-capture :wk "capture... (roam)")
  "l" '(org-store-link :wk "store link")
  "s" '(org-roam-search :wk "search...")
  "f" '(org-funcs-roam-node-find :wk "roam file...")

  "r" '(nil :wk "review...")
  "rd" '(org-roam-review-list-recently-added :wk "recently added")
  "rn" '(citar-open-notes :wk "lit notes...")
  "rb" '(citar-open :wk "open reference...")
  "rr" '(org-roam-review :wk "review notes")
  "rt" '(org-roam-search-tags :wk "search (tags)...")
  "rl" '(org-roam-links :wk "show links")
  "rs" '(org-roam-slipbox-list-notes :wk "list (slipbox)...")

  "g" '(org-capture-goto-last-stored :wk "last captured")
  "t" '(org-funcs-todo-list :wk "todo list")
  "v" '(org-tags-view :wk "tags")
  "w" '(timekeep-visit-node :wk "work"))



;;; p - Projects

(leader-set-key :infix "p"
  "" '(nil :wk "projects")
  "!" '(project-async-shell-command :wk "shell command...")
  "c" '(project-compile :wk "compile...")
  "p" '(project-switch-project :wk "switch...")
  "f" '(project-find-file :wk "find file...")
  "d" '(project-find-dir :wk "find dir...")
  "b" '(project-switch-to-buffer :wk "switch buffer...")
  "D" '(project-dired :wk "dired")
  "/" '(consult-ripgrep :wk "search (rg)")
  "r" '(project-query-replace-regexp :wk "replace"))


;;; r - registers & bookmarks

;; TODO: remap the normal register binding to this prefix.



;;; t - Toggles

(leader-set-key :infix "t"
  "" '(nil :wk "toggle")
  "i" '(toggle-input-method :wk "input method")
  "c" '(hide/show-comments-toggle :wk "comments")
  "m" '(global-hide-mode-line-mode :wk "mode line"))



;;; w - Window management

(defun cb-split-window-horizontally-dwim (&optional arg)
  "When splitting window, show the other buffer in the new window.

With prefix arg ARG, don't select the new window."
  (interactive "P")
  (split-window-horizontally)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer))
    (unless arg
      (select-window target-window))))

(defun cb-split-window-vertically-dwim (&optional arg)
  "When splitting window, show the other buffer in the new window.

With prefix arg ARG, don't select the new window."
  (interactive "P")
  (split-window-vertically)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (other-buffer))
    (unless arg
      (select-window target-window))))

(defun cb-toggle-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(defun cb-delete-nondedicated-windows ()
  "Delete windows that are not dedicated."
  (interactive)
  (let ((windows (window-list))
        (selected (selected-window)))
    (dolist (win windows)
      (unless (or (equal win selected)
                  (window-dedicated-p win))
        (delete-window win)))))

(leader-set-key :infix "w"
  "" '(nil :wk "window")
  "SPC" '(window-toggle-side-windows :wk "toggle side windows")
  "p" '(evil-window-prev :wk "prev")
  "w" '(evil-window-next :wk "next")
  "n" '(evil-window-next :wk "next")
  "r" '(evil-window-rotate-downwards :wk "rotate")
  "/" '(cb-split-window-horizontally-dwim :wk "split (horizontal)")
  "-" '(cb-split-window-vertically-dwim :wk "split (vertical)")
  "=" '(balance-windows :wk "balance")
  "d" '(delete-window :wk "delete")
  "o" '(cb-delete-nondedicated-windows :wk "delete non-dedicated")
  "O" '(delete-other-windows :wk "delete others")
  "t" '(cb-toggle-window-dedication :wk "toggle dedication"))



;;; y - Text snippets

(leader-set-key :infix "y"
  "" '(nil :wk "snippets")
  "n" '(yas-new-snippet :wk "new")
  "e" '(yas-expand :wk "expand")
  "f" '(yas-visit-snippet-file :wk "open...")
  "y" '(yas-insert-snippet :wk "insert..."))



;;; z - Text scale

(leader-set-key :infix "z"
  "" '(nil :wk "zoom")
  "+" '(default-text-scale-increase :wk "increase text scale")
  "-" '(default-text-scale-decrease :wk "decrease text scale")
  "=" '(default-text-scale-reset :wk "reset text scale"))

(provide 'cb-leader)

;;; cb-leader.el ends here
