;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Declares some variables and bootstraps the rest of the configuration.
;;
;; One main difference from other configurations out there is that I use git subtrees for
;; many core packages, instead of relying on the Emacs package manager.

;;; Code:

(unless noninteractive
  (message "Loading %s..." load-file-name))


;; Print overall startup time.

(defconst emacs-start-time (current-time))

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))


;; Bootstrap use-package.

(require 'seq)

(let ((toplevel-dirs
       (seq-map (lambda (path)
                  (expand-file-name path user-emacs-directory))
                '("config" "lisp")))
      (git-subtrees
       (seq-remove #'file-directory-p
                   (directory-files "lisp" t "^[^.]"))))

  (dolist (path (concat toplevel-dirs git-subtrees))
    (add-to-list 'load-path path)))

(defconst use-package-verbose t)
(require 'use-package)


;; Load features.

(use-package cb-emacs)
(use-package cb-basic-settings)
(use-package cb-evil)

(provide 'init)

;;; init.el ends here
