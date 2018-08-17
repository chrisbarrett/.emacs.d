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



;; eshell implements a shell in emacs lisp.

(use-package eshell
  :commands (eshell)
  :config
  (progn
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
