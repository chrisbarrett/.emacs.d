;;; cb-eshell-funcs.el --- Eshell commands  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'eshell)
(require 'f)
(require 'subr-x)

(autoload 'eshell/cd "em-dirs")
(autoload 'magit-status "magit")
(autoload 'magit-toplevel "magit")
(autoload 'nvm--installed-versions "nvm")
(autoload 'nvm-use "nvm")

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

(defun cb-eshell-funcs--nvm-version-from-nvmrc (dir)
  (or (-some--> (locate-dominating-file dir ".nvmrc")
                (f-join it ".nvmrc")
                (string-trim (f-read-text it)))
      (user-error "No .nvmrc file found")))

(defun eshell/npm (&rest args)
  (unless (bound-and-true-p nvm-current-version)
    (nvm-use (cb-eshell-funcs--read-nvm-version)))
  (when (bound-and-true-p nvm-current-version)
    (-let [(_ nvm) nvm-current-version]
      (eshell-do-eval
       (eshell-parse-command (f-join nvm "bin" "npm") args)
       t))))

(defun cb-eshell-funcs--read-nvm-version ()
  (let ((default (ignore-errors
                   (cb-eshell-funcs--nvm-version-from-nvmrc default-directory)))
        (versions (sort (seq-map #'car (nvm--installed-versions))
                        #'string<)))
    (completing-read "Node version: "
                     (if default (seq-uniq (cons default versions)) versions)
                     nil t)))

(defun eshell/nvm-use (&rest args)
  (let ((version (pcase (-flatten args)
                   ('nil
                    (or (cb-eshell-funcs--nvm-version-from-nvmrc default-directory)
                        (cb-eshell-funcs--read-nvm-version)))
                   (`(,version)
                    version)
                   (_
                    (user-error "Usage: nvm use <version>")))))
    (nvm-use (format "%s" version))
    (eshell-printn (format "Using node version %s" version))))

(defun eshell/nvm (&rest args)
  (pcase (-flatten args)
    (`("use" . ,args)
     (eshell/nvm-use args))
    (`(,(or "ls" "list") . ,_)
     (sort (seq-map #'car (nvm--installed-versions))
           #'string<))
    (`(,command . ,_)
     (user-error "Unknown nvm command: %s" command))
    ('nil
     (user-error (string-trim "
Usage: nvm use [version]
       nvm ls
")))))

(provide 'cb-eshell-funcs)

;;; cb-eshell-funcs.el ends here
