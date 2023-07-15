;;; cb-search.el --- Search & replace  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)

(use-package deadgrep
  :ensure t
  :general (:keymaps 'deadgrep-mode-map "C-c C-w" #'deadgrep-edit-mode)
  :init
  (defalias 'rg #'deadgrep)
  :config
  (setq-default deadgrep--search-type 'regexp))

(use-package wgrep
  :ensure t)

(use-package consult
  :ensure t
  :custom
  (consult-ripgrep-args '("rg"
                          "--follow"
                          "--null"
                          "--line-buffered"
                          "--color=never"
                          "--max-columns=1000"
                          "--path-separator /"
                          "--smart-case"
                          "--no-heading"
                          "--with-filename"
                          "--line-number"
                          "--search-zip")))

(provide 'cb-search)

;;; cb-search.el ends here
