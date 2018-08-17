;;; config-eshell.el --- Configuration for eshell.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'f)
(require 'general)
(require 'paths)
(require 'dash)

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
    (add-hook 'eshell-mode-hook #'config-eshell-setup-keybindings)

    ;; keep aliases under etc directory, which is tracked by git.

    (f-mkdir config-eshell-etc-directory)
    (general-setq eshell-aliases-file (f-join config-eshell-etc-directory  "aliases"))))

(use-package em-smart
  :hook (eshell-mode . eshell-smart-initialize))

;; fasd teaches Emacs to update to fasd cache as files and dirs are opened.

(use-package fasd
  :straight
  (:type git :repo "https://framagit.org/steckerhalter/emacs-fasd.git")
  :config
  (progn
    ;; Add recentf list to fasd DB.
    (apply #'start-process "*fasd*" nil "fasd" "--add" (seq-map #'shell-quote-argument recentf-list))
    (global-fasd-mode +1)))



;; Define some eshell commands

(autoload 'eshell/cd "em-dirs")

(defun eshell/j (&rest query)
  "Change to a directory using fasd with QUERY."
  (unless query
    (user-error "Usage error: must supply a query"))
  (-let* ((query-string (string-join query " "))
          (results (shell-command-to-string (format "fasd -l -R -d %s" (shell-quote-argument query-string)))))
    (-if-let* (((dir) (split-string results "\n" t)))
        (eshell/cd dir)
      (user-error "No results"))))

(provide 'config-eshell)

;;; config-eshell.el ends here
