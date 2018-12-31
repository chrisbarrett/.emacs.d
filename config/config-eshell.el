;;; config-eshell.el --- Configuration for eshell.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'f)
(require 'general)
(require 'paths)
(require 'pusheen)

(defconst config-eshell-etc-directory (f-join paths-etc-directory "eshell"))
(autoload 'evil-local-set-key "evil-core")
(autoload 'page-break-lines-mode "page-break-lines")

;; NOTE: Load just this feature, instead of all of magit.
(autoload 'magit-get-current-branch "magit-git")
(autoload 'magit-process-file "magit-process")

(autoload 'display-buffer-fullframe "display-buffer-fullframe")
(autoload 'page-break-lines--update-display-table "page-break-lines")



(defvar config-eshell--changing-dir-interactively nil)

;; eshell implements a shell in emacs lisp.

(use-package eshell
  :commands (eshell)

  :preface
  (progn
    ;; HACK eshell mode map is set as a local variable in its mode function.
    ;; deep cry. ( -̩̩̩͡˛ -̩̩̩͡ )
    (defun config-eshell-setup-keybindings ()
      (evil-local-set-key 'insert (kbd "C-e") 'end-of-line)
      (evil-local-set-key 'insert (kbd "C-a") 'eshell-bol)))

  :config
  (progn
    (require 'eshell-hacks)
    (general-setq eshell-modules-list
                  '(eshell-tramp
                    eshell-alias
                    eshell-banner
                    eshell-basic
                    eshell-cmpl
                    eshell-dirs
                    eshell-glob
                    eshell-hist
                    eshell-ls
                    eshell-pred
                    eshell-prompt
                    eshell-script
                    eshell-term
                    eshell-unix))

    ;; Customise the prompt header.

    (setq eshell-banner-message
          (let* ((pad-char "\u0080")
                 (str
                  (format "%s%s\n%s%s\n\n" pad-char (pusheen 'winky)
                          pad-char
                          (propertize " O hai!" 'face '(:height 400)))))
            (propertize str 'read-only t)))
    (add-hook 'eshell-mode-hook #'config-eshell-setup-keybindings)
    (add-hook 'eshell-mode-hook #'pusheen-animate-all)

    ;; keep aliases under etc directory, which is tracked by git.

    (f-mkdir config-eshell-etc-directory)
    (general-setq eshell-aliases-file (f-join config-eshell-etc-directory  "aliases"))))

;; fasd teaches Emacs to update to fasd cache as files and dirs are opened.

(use-package fasd
  :straight
  (:type git :repo "https://framagit.org/steckerhalter/emacs-fasd.git")
  :config
  (progn
    ;; Add recentf list to fasd DB.
    (apply #'start-process "*fasd*" nil "fasd" "--add" (seq-map #'shell-quote-argument recentf-list))
    (global-fasd-mode +1)))

;; pretty-eshell defines some utility fns for building a nice prompt.

(use-package pretty-eshell
  :after eshell
  :preface
  (progn
    (defface eshell-dimmed
      '((t :inherit default))
      "Face for dimmed text in eshell."
      :group 'config-eshell)

    (defface eshell-timestamp
      '((t :inherit eshell-dimmed :height 0.7))
      "Face for timestamps in eshell."
      :group 'config-eshell)

    (defun config-eshell--dim-commands-on-submission ()
      (let ((start eshell-last-output-start)
            (end (line-end-position)))
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char start)
            (search-forward "\u000c")
            (put-text-property (point) end 'face 'eshell-dimmed)
            (put-text-property (point) end 'read-only t)
            (put-text-property (point) end 'rear-nonsticky t)))))

    (defun config-eshell--inhibit-submission-on-empty (f &rest args)
      ;; Always run command if this is a dir change from hyrda.
      (if config-eshell--changing-dir-interactively
          (apply f args)
        (let* ((start eshell-last-output-end)
               (end (line-end-position))
               (input (buffer-substring-no-properties start end)))
          (if (string-empty-p (string-trim input))
              (delete-region start end)
            (config-eshell--dim-commands-on-submission)
            (apply f args))))))

  :config
  (progn
    ;; Show a horizontal rule and timestamp between commands.

    (defvar-local config-eshell--previous-time nil)

    (setq pretty-eshell-header-fun
          (let ((page-break "\u000c")
                (horizontal-tab "\u0009"))
            (lambda ()
              (let* ((time (format-time-string "%H:%M" (current-time)))
                     (timestamp (concat
                                 horizontal-tab
                                 (propertize time 'face 'eshell-timestamp)))
                     (str (propertize (concat timestamp "\n" page-break "\n")
                                      'read-only t)))
                (prog1 str
                  (setq config-eshell--previous-time time))))))

    (add-hook 'eshell-mode-hook 'page-break-lines-mode)

    ;; Prevent command submission if there's no text to submit.

    (advice-add 'eshell-send-input :around #'config-eshell--inhibit-submission-on-empty)

    ;; Change the prompt, depending on the previous command's exit code.

    (setq eshell-prompt-function 'pretty-eshell-prompt-func)
    (setq pretty-eshell-prompt-string-fun (lambda ()
                                            (concat " " (if (eshell-exit-success-p)
                                                            ">"
                                                          (propertize "✘" 'face 'error))
                                                    " ")))
    (setq eshell-prompt-regexp (rx bol (* space) (or ">" "✘") space))

    ;; Directory
    (pretty-eshell-define-section config-eshell-dir
      ""
      (abbreviate-file-name (eshell/pwd))
      '(:inherit eshell-ls-directory :weight light))

    ;; Git Branch
    (pretty-eshell-define-section config-eshell-git
      ""
      (when-let* ((branch (magit-get-current-branch)))
        (s-truncate 20 (s-chop-prefixes '("feature/" "release/") branch)))
      '(:foreground "#cb4b16" :weight light))

    ;; Node version
    (pretty-eshell-define-section config-eshell-node
      ""
      (when (and (bound-and-true-p nvm-current-version)
                 (locate-dominating-file default-directory ".nvmrc"))
        (car nvm-current-version))
      '(:foreground "#d33682" :weight light))

    (setq pretty-eshell-funcs (list config-eshell-dir config-eshell-git config-eshell-node))))


;; Horrific hacks to align buffer objects via control chars:
;;
;; 1. right-align the timestamp in the eshell prompt using C-i control
;; character.
;;
;; 2. center-align the pusheen hero image.
;;
;; Adapted from the implementation of page-break-lines.

(defun config-eshell--align-timestamp ()
  (let* ((space-char 32)
         (timestamp-width 5) ; HH:MM
         (spaces-count (- (1+ (window-width)) timestamp-width))
         (width (* (char-width space-char) spaces-count))
         (new-display-entry (vconcat (make-list width space-char))))
    (unless (equal new-display-entry (elt buffer-display-table ?\^I))
      (aset buffer-display-table ?\^I new-display-entry))))

(defun config-eshell--align-pusheen ()
  (-let* ((pad-control-char ?\u0080)
          ((pusheen-cols . _y)
           (image-size (alist-get 'winky pusheen-image-alist)))
          (space-char 32)
          (spaces-count (/ (- (1+ (window-width))
                              pusheen-cols)
                           2))
          (width (truncate (* (char-width space-char) spaces-count))))
    (when (plusp width)
      (let ((new-display-entry (vconcat (make-list width space-char))))
        (unless (equal new-display-entry
                       (elt buffer-display-table pad-control-char))
          (aset buffer-display-table pad-control-char new-display-entry))))))

(defun eshell-timestamp--update-display-table (window)
  (with-current-buffer (window-buffer window)
    (with-selected-window window
      (let ((inhibit-read-only t))
        (page-break-lines--update-display-table window)
        (unless buffer-display-table
          (setq buffer-display-table (make-display-table)))
        (config-eshell--align-timestamp)
        (config-eshell--align-pusheen)))))

(defun eshell-timestamp--update-display-tables  (&optional frame)
  (dolist (window (window-list frame 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'eshell-mode)
        (eshell-timestamp--update-display-table window)))))

(defun eshell-timestamp--configure-hooks ()
  (dolist (hook '(window-configuration-change-hook
                  window-size-change-functions
                  after-setting-font-hook))
    (add-hook hook 'eshell-timestamp--update-display-tables t t)))

(add-hook 'eshell-mode-hook #'eshell-timestamp--configure-hooks)

(defun config-eshell--inhibit-read-only (f &rest args)
  (let ((inhibit-read-only t))
    (apply f args)))

(advice-add 'eshell-output-filter :around #'config-eshell--inhibit-read-only)



;; proced provides a top-like process manager.

(use-package proced
  :commands (proced)
  :general
  (:keymaps 'proced-mode-map
   :states 'normal
   "r" 'proced-refine
   "R" 'proced-renice)
  :config
  (progn
    (setf (nth 1 (alist-get 'comm proced-grammar-alist))
          "%-20s")

    (setq-default proced-format
                  '(comm pid state pcpu vsize pmem user))))

;; eshell functions are defined in this lib.

(use-package cb-eshell-funcs
  :after eshell)

;; Prodigy provides a UI for managing external processes.

(use-package prodigy
  :straight t
  :commands (prodigy)
  :preface
  (progn
    (defun prodigy-start-with-tag (tag)
      "Start all services with TAG."
      (interactive (list (intern (completing-read "Start processes with tag: " (prodigy-tags)))))
      (prodigy)
      (if-let ((services (prodigy-services-tagged-with tag)))
          (dolist (service services)
            (prodigy-start-service service))
        (user-error "No services for tag")))

    (defun prodigy-stop-with-tag (tag)
      "Start all services with TAG."
      (interactive (list (intern (completing-read "Stop processes with tag: " (prodigy-tags)))))
      (prodigy)
      (if-let ((services (prodigy-services-tagged-with tag)))
          (dolist (service services)
            (prodigy-stop-service service))
        (user-error "No services for tag"))))

  :general
  (:states 'motion :keymaps 'prodigy-mode-map
   "TAB" #'prodigy-display-process
   "gr" #'prodigy-refresh
   "t" #'prodigy-start-with-tag
   "T" #'prodigy-stop-with-tag)
  :config
  (progn
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*prodigy*" eos)
                   (display-buffer-reuse-window
                    display-buffer-fullframe)
                   (reusable-frames . visible)))

    ;; Truncate buffers.
    (setq prodigy-view-truncate-by-default t)

    ;; Use standard completing-read.
    (setq prodigy-completion-system 'default)))

;; Custom command to for hydra eshell command

(defun cb-eshell-at-dir (&optional arg)
  "Open eshell. With ARG, change to buffer's directory."
  (interactive "P")
  (if-let* ((dir default-directory)
            (eshell-buf (get-buffer "*eshell*")))
      (with-current-buffer eshell-buf
        (when arg
          (let ((config-eshell--changing-dir-interactively t))
            (eshell/cd dir)
            (eshell-reset t)))
        (pop-to-buffer eshell-buf))
    (eshell)))

(defun config-eshell-raise ()
  "Utility function for emacsclient to pop open an eshell."
  (with-current-buffer (eshell)
    (goto-char (point-max))
    (when (bound-and-true-p evil-mode)
      (evil-append-line nil))))

(provide 'config-eshell)

;;; config-eshell.el ends here
