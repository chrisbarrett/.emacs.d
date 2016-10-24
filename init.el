;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Declares some variables and bootstraps the rest of the configuration.
;;
;; One main difference from other configurations out there is that I use git subtrees for
;; many core packages, instead of relying on the Emacs package manager.

;;; Code:

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))


;; Bootstrap use-package.

(require 'seq)

(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
       (config-dir (expand-file-name "config" user-emacs-directory))
       (git-subtrees
        (seq-filter #'file-directory-p
                    (directory-files lisp-dir t "^[^.]")))
       (config-subtrees
        (seq-filter #'file-directory-p
                    (directory-files config-dir t "^[^.]"))))
  (dolist (path (append (list lisp-dir config-dir) config-subtrees git-subtrees))
    (add-to-list 'load-path path)
    (add-to-list 'load-path (concat path "/lisp"))))

(defconst use-package-verbose t)
(require 'use-package)


;; Load features.

(use-package cb-emacs)
(use-package cb-basic-settings)
(use-package cb-modeline)
(use-package cb-which-key)
(use-package cb-leader-keys)
(use-package cb-evil)
(use-package cb-ivy)
(use-package cb-avy)
(use-package cb-darwin :if (equal system-type 'darwin))
(use-package cb-ag)
(use-package cb-projectile)
(use-package cb-elisp)
(use-package cb-smartparens)
(use-package cb-magit)
(use-package cb-company)
(use-package cb-autoinsert)
(use-package cb-undo-tree)
(use-package cb-ws-butler)
(use-package cb-yasnippet)
(use-package cb-flycheck)
(use-package cb-aggressive-indent)
(use-package cb-server)
(use-package cb-hexl)
(use-package cb-volatile-highlights)
(use-package cb-info)
(use-package cb-highlight-todo)
(use-package cb-neotree)

(use-package personal-config
  :load-path "~/Dropbox/emacs")


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
