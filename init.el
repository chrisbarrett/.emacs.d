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

(let ((toplevel-dirs
       (seq-map (lambda (path)
                  (expand-file-name path user-emacs-directory))
                '("config" "lisp")))
      (git-subtrees
       (seq-filter #'file-directory-p
                   (directory-files "lisp" t "^[^.]")))
      (config-subtrees
       (seq-filter #'file-directory-p
                   (directory-files "config" t "^[^.]"))))

  (dolist (path (append toplevel-dirs config-subtrees git-subtrees))
    (add-to-list 'load-path path)))

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
(use-package cb-darwin :if (equal system-type 'darwin))
(use-package cb-ag)


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
