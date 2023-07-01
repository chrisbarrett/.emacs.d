;;; cb-search.el --- Search & replace  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package deadgrep
  :general (:keymaps 'deadgrep-mode-map "C-c C-w" #'deadgrep-edit-mode)
  :init
  (defalias 'rg #'deadgrep)
  :config
  (setq-default deadgrep--search-type 'regexp))

(use-package wgrep)

(provide 'cb-search)

;;; cb-search.el ends here
