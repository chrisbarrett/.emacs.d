;;; init.el --- Emacs configuration entrypoint  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "27")
  (error "This version of Emacs is not supported"))

;; This is inevitable.
(require 'cl)
(require 'subr-x)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq gc-cons-threshold 8000000)
            (setq gc-cons-percentage 0.7)

            (add-function :after after-focus-change-function
              (lambda ()
                (unless (frame-focus-state)
                  (garbage-collect))))))


;; Load some core packages

(with-no-warnings
  (setq use-package-verbose t))

(eval-when-compile
  (require 'use-package))

(require 'general)

(use-package major-mode-hydra
  :demand t
  :general (:states '(normal motion) "," #'major-mode-hydra)
  :preface
  (progn
    (autoload 'all-the-icons-icon-for-mode "all-the-icons")

    (defun init--major-mode-hydra-title-generator (mode)
      (let* ((icon (all-the-icons-icon-for-mode mode :v-adjust -0.15))
             (mode-title (string-remove-suffix "-mode" (symbol-name mode)))
             (components
              (seq-filter #'stringp
                          (list icon (s-titleized-words mode-title) "Commands"))))
        (string-join components " "))))

  :custom ((major-mode-hydra-invisible-quit-key "q")
           (major-mode-hydra-title-generator #'init--major-mode-hydra-title-generator)))


;; Load features.

(require 'paths (expand-file-name "paths.el" user-emacs-directory))
(paths-initialise)
(add-to-list 'custom-theme-load-path paths-themes-directory)

;; no-littering overrides many common paths to keep the .emacs.d directory
;; clean.
;;
;; Load it here since we want to refer to path vars, and need to make sure it's
;; loaded very early in the startup process.

(use-package no-littering
  :demand t
  :init
  (progn
    (setq no-littering-etc-directory paths-etc-directory)
    (setq no-littering-var-directory paths-cache-directory))
  :config
  (progn
    (setq auto-save-file-name-transforms
          `((".*" ,(expand-file-name "auto-save" paths-cache-directory) t)))

    (eval-when-compile
      (require 'recentf))

    (with-eval-after-load 'recentf
      (add-to-list 'recentf-exclude no-littering-etc-directory)
      (add-to-list 'recentf-exclude no-littering-var-directory))))


;; Load OS-specific configuration.

(use-package config-darwin-os
  :if (equal system-type 'darwin)
  :demand t)

(use-package config-nixos-os
  :if (equal system-type 'gnu/linux)
  :demand t)

;; Load host-specific overrides for vars.

(when (file-exists-p paths-personal-config)
  (load-file paths-personal-config))

(when (file-exists-p paths-hostfile)
  (load-file paths-hostfile))


;; Load some config files first for consistency.

(use-package config-basic-settings)
(use-package config-evil)

;; Load remaining config files.

(dolist (feature (seq-reduce (lambda (acc it)
                               (if (and (string-suffix-p ".el" it)
                                        (not (string-suffix-p "-os.el" it)))
                                   (cons (intern (string-remove-suffix ".el" it))
                                         acc)
                                 acc))
                             (directory-files paths-config-directory)
                             nil))
  (eval `(use-package ,feature :demand t)))

;; Support loading files in nursery

(dolist (dir (f-directories paths-nursery-directory))
  (add-to-list 'load-path dir))

;; Load files in org lisp directory

(with-eval-after-load 'org
  (when (file-directory-p paths-org-lisp-directory)
    (load (expand-file-name "init.el" paths-org-lisp-directory) t)))

;; Load files in ledger directory

(when (file-directory-p paths-ledger-lisp-directory)
  (load (expand-file-name "init.el" paths-ledger-lisp-directory) t))


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

;;; init.el ends here
