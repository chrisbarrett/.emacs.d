;;; cb-eshell-funcs.el --- Eshell commands  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(autoload 'magit-status "magit")
(autoload 'magit-toplevel "magit")
(autoload 'eshell/cd "em-dirs")

(defun cb-eshell-funcs--fasd-dir (query)
  (let ((result (shell-command-to-string (format "fasd -l -R -d %s" (shell-quote-argument query)))))
    (or (car (split-string result "\n" t))
        (user-error "No results"))))

(defun eshell/j (&rest query)
  "Change to a directory using fasd with QUERY."
  (unless query
    (user-error "Usage error: must supply a query"))
  (eshell/cd (cb-eshell-funcs--fasd-dir (string-join query " "))))

(defun eshell/g (&rest query)
  "Open magit, optionally using fasd QUERY to find repo directory."
  (when query
    (apply 'eshell/j query))
  (magit-status))

(provide 'cb-eshell-funcs)

;;; cb-eshell-funcs.el ends here
