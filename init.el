;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Declares some variables and bootstraps the rest of the configuration.
;;
;; One main difference from other configurations out there is that I use git subtrees for
;; many core packages, instead of relying on the Emacs package manager.

;;; Code:

(setq gc-cons-threshold (* 800 1024))

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setenv "INSIDE_EMACS" "true")

;; Initialize package.el
;;
;; Most packages are installed using git subtrees, but some packages (such as
;; flycheck) break unless installed via package.el.

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Ungh. Compatability macro definitions.

(require 'subr-x)

(eval-when-compile
  (when (version< emacs-version "26")
    (defalias 'if-let* 'if-let)
    (defalias 'when-let* 'when-let)

    (put 'if-let* 'lisp-indent-function 2)
    (put 'when-let* 'lisp-indent-function 1)))

;; Bootstrap use-package.

(require 'seq)

(defun cb-init/init-load-path (&optional interactive-p)
  "Add select subdirs of `user-emacs-directory' to the `load-path'.

If argument INTERACTIVE-P is set, log additional information."
  (interactive "p")
  (let* ((before load-path)
         (lisp-dir (expand-file-name "lisp" user-emacs-directory))
         (config-dir (expand-file-name "config" user-emacs-directory))
         (git-subtrees
          (seq-filter #'file-directory-p
                      (directory-files lisp-dir t "^[^.]")))
         (config-subtrees
          (seq-filter #'file-directory-p
                      (directory-files config-dir t "^[^.]"))))
    (dolist (path (append (list lisp-dir config-dir) config-subtrees git-subtrees))
      (add-to-list 'load-path path)
      (add-to-list 'Info-default-directory-list path)
      (add-to-list 'load-path (concat path "/emacs"))
      (add-to-list 'load-path (concat path "/elisp"))
      (add-to-list 'load-path (concat path "/lisp")))

    (add-to-list 'load-path (concat lisp-dir "/org-mode/contrib/lisp"))
    (add-to-list 'load-path (concat lisp-dir "/gocode/emacs-company"))

    (setq load-path (seq-filter #'file-directory-p load-path))
    (setq Info-default-directory-list (seq-filter #'file-directory-p Info-default-directory-list))

    (when interactive-p
      (if-let* ((added (seq-difference load-path before)))
          (message "Load path updated. Added: %S" added)
        (message "No change to load-path")))))

(cb-init/init-load-path)
(defconst use-package-verbose t)
(require 'use-package)


;; Load features.

(use-package cb-emacs)

(use-package cb-faces)
(use-package cb-basic-settings)
(use-package cb-modeline)
(use-package cb-auto-save)
(use-package cb-leader-keys)
(use-package cb-evil)
(use-package cb-ivy)
(use-package cb-avy)
(use-package cb-darwin :if (equal system-type 'darwin))
(use-package cb-dumb-jump)
(use-package cb-search)
(use-package cb-projectile)
(use-package cb-elisp)
(use-package cb-smartparens)
(use-package cb-git)
(use-package cb-company)
(use-package cb-autoinsert)
(use-package cb-undo-tree)
(use-package cb-ws-butler)
(use-package cb-yasnippet)
(use-package cb-parentheses)
(use-package cb-flycheck)
(use-package cb-aggressive-indent)
(use-package cb-server)
(use-package cb-hexl)
(use-package cb-volatile-highlights)
(use-package cb-info)
(use-package cb-highlight-todo)
(use-package cb-neotree)
(use-package cb-mu4e)
(use-package cb-ahs)
(use-package cb-org)
(use-package cb-ledger)
(use-package cb-scala)
(use-package cb-groovy)
(use-package cb-rust)
(use-package cb-ibuffer)
(use-package cb-yaml)
(use-package cb-go)
(use-package cb-dired)
(use-package cb-spelling)
(use-package cb-diff)
(use-package cb-coffeescript)
(use-package cb-web-mode)
(use-package cb-markdown)
(use-package cb-restclient)
(use-package cb-calc)
(use-package cb-haskell)
(use-package cb-shell)
(use-package cb-csv)
(use-package cb-highlight-thing)
(use-package cb-idris)
(use-package cb-docker)
(use-package cb-graphviz)
(use-package cb-protobuf)
(use-package cb-latex)
(use-package cb-python)
(use-package cb-gud)
(use-package cb-eyebrowse)
(use-package cb-nix)
(use-package cb-tiny)
(use-package cb-fstar)

(use-package personal-config
  :load-path "~/Sync/emacs")

(unless user-full-name (warn "`user-full-name' not set"))
(unless user-mail-address (warn "`user-mail-address' not set"))

;;; Post init setup.

(unless (file-directory-p org-directory)
  (when (y-or-n-p (format "`org-directory' does not exist. Create at %s? " org-directory))
    (mkdir org-directory)))

;;; Print overall startup time.

(unless noninteractive
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))


(provide 'init)

;;; init.el ends here
