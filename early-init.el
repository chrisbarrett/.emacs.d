;;; early-init.el --- Set critical runtime variables  -*- lexical-binding: t; -*-

;;; Commentary:

;; early-init.el is used to set certain variables very early in the startup
;; sequence. This Lisp is evaluated before the graphical context and package.el
;; are initialised.

;;; Code:

(defconst emacs-start-time (current-time)
  "The time at which this instance of Emacs was started.")

;; Set an environment variable so child processes can detect that they're inside
;; Emacs

(setenv "INSIDE_EMACS" "true")

;; Prefer more recent Lisp files to outdated ELC files when loading.
(setq load-prefer-newer t)



;;; Crank up the max size of subprocess output to read.

;; This is needed for language servers to perform well, since they communicate
;; using large chunks of JSON.

;; There is work underway to understand what a better default setting would be.

(setq read-process-output-max (* 1024 1024))

;;; Increase GC limit to reduce collections during startup

;; Garbage collection runs slow down the Emacs startup sequence. Temporarily
;; increase settings. We will set final values at the end of the startup sequence.

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.7)

;; See:
;; - https://emacs-lsp.github.io/lsp-mode/page/performance/
;; - https://yhetil.org/emacs-devel/87v8j6t3i9.fsf@localhost/
;; - https://www.reddit.com/r/emacs/comments/14dej62



;;; Make this checkout directory the value for `user-emacs-directory'.

;; This makes it possible to run a version of this config checked out to a git
;; worktree, for instance, and have all lisp config resolved relative to this
;; early-init file.

(let ((this-file (or load-file-name (buffer-file-name))))
  (setq user-emacs-directory (file-name-directory this-file)))



;;; Disable package.el to improve startup time

;; package.el increases startup time, and I use the Nix Emacs overlay to handle
;; package installation.

(setq package-enable-at-startup nil)

;; See:
;; - https://github.com/nix-community/emacs-overlay



;;; Put the ELN cache directory in an XDG-conformant place

(let ((cache-dir "~/.cache/emacs/eln-cache/"))
  (mkdir cache-dir t)
  (add-to-list 'native-comp-eln-load-path cache-dir))



;;; Window manager compat

;; Enable pixel-wise frame resizing. This helps tiling window managers apply the
;; right size to Emacs frames.

(setq frame-resize-pixelwise t)

;; Show just 'Emacs' for window manager title bar

(setq frame-title-format "Emacs")



;;; General GUI settings

;; Disable menu bars and other GUI elements I don't want to use

(custom-set-variables '(menu-bar-mode nil)
                      '(tool-bar-mode . nil)
                      '(scroll-bar-mode nil))

(modify-all-frames-parameters '((vertical-scroll-bars)))

;; Make the title bar more natural in macOS

(when (equal system-type 'darwin)
  (setq ns-use-proxy-icon nil)
  (modify-all-frames-parameters '((ns-appearance . dark)
                                  (ns-transparent-titlebar . t))))



;;; Set source directory

;; Set the location where Emacs looks for its C source files. This is needed to
;; view the definitions of special forms implemented in C.

;; The path to sources in the nix-store is injected via an environment variable.

(setq source-directory (expand-file-name emacs-version (getenv "NIX_EMACS_SRC_DIR")))



;;; Set PATH correctly on Darwin

;; macOS prevents PATH being modified for graphical apps, so the wrapper set up
;; with Nix won't work. Use another environment variable to pass through the
;; desired value.

(autoload 'string-join "subr-x")

(when (equal system-type 'darwin)
  (dolist (dir (split-string (getenv "NIX_EMACS_DARWIN_PATH_EXTRAS") ":"))
    (push dir exec-path))
  (setq exec-path (seq-uniq exec-path))

  (setenv "PATH" (string-join exec-path ":")))



;;; Configure use-package

(require 'cb-ensured-packages (expand-file-name "config/cb-ensured-packages" user-emacs-directory))

(with-no-warnings
  (setq use-package-ensure-function #'cb-ensured-packages-record)
  (setq use-package-always-defer t)
  (setq use-package-minimum-reported-time 0.05)
  (setq use-package-compute-statistics t)
  (setq use-package-verbose (not noninteractive)))



;;; Never ever create files in undesireable places.

(use-package no-littering :ensure t :demand t
  :custom
  (no-littering-var-directory (expand-file-name "~/.cache/emacs"))
  :autoload
  (no-littering-theme-backups)
  :config
  (no-littering-theme-backups))



;;; Set some sane defaults.

(setq ring-bell-function #'ignore)
(setq use-short-answers t)
(setq delete-by-moving-to-trash nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Disable backup files, lockfiles, etc.
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Inhibit the default startup screen.
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq initial-major-mode 'fundamental-mode)

;; Never show the useless hello file.
(defalias #'view-hello-file #'ignore)

;; Don't nag when following symlinks to files under version control.
(setq vc-follow-symlinks t)

;; Always insert a final newline, as per the Unix convention.
(setq require-final-newline t)

;; Harden network settings.
(with-eval-after-load 'nsm
  (with-no-warnings
    (setq network-security-level 'high)
    (setq nsm-noninteractive t)))

;; Disable warnings from obsolete advice system. These are caused by packages
;; and are generally not actionable by me.
(setq ad-redefinition-action 'accept)

;; Write customisations applied via `custom.el' to a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; early-init.el ends here
