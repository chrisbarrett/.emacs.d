;;; config-eshell.el --- Configuration for eshell.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'eshell-hacks)
(require 'f)
(require 'general)
(require 'paths)

(defconst config-eshell-etc-directory (f-join paths-etc-directory "eshell"))
(autoload 'evil-local-set-key "evil-core")



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
    (require 'pusheen)
    (setq eshell-banner-message (format "%13s\n%15s\n\n" (pusheen 'winky)
                                        (propertize "O hai!" 'face '(:height 400))))
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
  (defun config-eshell--inhibit-submission-on-empty (f &rest args)
    (let* ((start eshell-last-output-end)
           (end (point))
           (input (buffer-substring-no-properties start end)))
      (if (string-empty-p (string-trim input))
          (delete-region start end)
        (apply f args))))

  :config
  (progn
    ;; Show a horizontal rule and timestamp between commands.

    (defvar-local config-eshell--previous-time nil)

    (setq pretty-eshell-header-fun
          (let ((page-break "\u000c"))
            (lambda ()
              (let* ((time (format-time-string " %H:%M" (current-time)))
                     (timestamp (s-pad-left (1- (window-width)) " "
                                            (propertize time 'face 'page-break-lines))))

                (prog1 (concat (if (equal time config-eshell--previous-time) "" (concat timestamp "\n"))
                               page-break "\n")
                  (setq config-eshell--previous-time time))))))

    (require 'page-break-lines)
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

    ;; Customise the prompt header.

    ;; Directory
    (pretty-eshell-define-section config-eshell-dir
      ""
      (abbreviate-file-name (eshell/pwd))
      '(:inherit eshell-ls-directory :weight light))

    ;; NOTE: Load just this feature, instead of all of magit.
    (autoload 'magit-get-current-branch "magit-git")
    (autoload 'magit-process-file "magit-process")

    ;; Git Branch
    (pretty-eshell-define-section config-eshell-git
      ""
      (magit-get-current-branch)
      '(:foreground "#cb4b16" :weight light))

    (setq pretty-eshell-funcs (list config-eshell-dir config-eshell-git))))

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

(provide 'config-eshell)

;;; config-eshell.el ends here
