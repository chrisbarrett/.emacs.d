;;; cb-projectile.el --- Configuration for projectile.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'subr-x))

(require 'dash-functional)
(require 'cb-paths)
(require 'spacemacs-keys)
(require 'cb-projectile-functions)

(use-package cb-projectile-functions
  :defines (cb-projectile-functions-ignored-base-dirs)
  :config
  (setq cb-projectile-functions-ignored-base-dirs
        '("/nix/store/"
          "~/.nvm/"
          "~/.ghc/"
          "~/.stack/"
          "~/.emacs.d/straight/"
          "~/.rustup/"
          "~/tmp/")))

(use-package projectile
  :straight t
  :commands (projectile-ag
             projectile-compile-project
             projectile-invalidate-cache
             projectile-mode
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-project
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-test-project)

  :functions (projectile-project-p)

  :preface
  (progn
    (autoload 'magit-status "magit")
    (autoload 'projectile-register-project-type "projectile")

    (defun cb-projectile--find-files-with-string-using-rg (fn string directory)
      (if (and (projectile-unixy-system-p) (executable-find "rg"))
          (let* ((search-term (shell-quote-argument string))
                 (cmd (concat "rg --fixed-strings --color=never --no-heading --files-with-matches -- " search-term)))

            (projectile-files-from-cmd cmd directory))
        (funcall fn string directory)))

    (defun cb-projectile--file-is-child-of-test-dir (&optional has-test-prefix-or-suffix)
      (or has-test-prefix-or-suffix
          (when-let* ((file (buffer-file-name)))
            (seq-contains (f-split file) "test"))))

    (defun cb-projectile--substitute-test-with-impl (&optional existing)
      (or existing
          (when-let* ((file (buffer-file-name))
                      (impl-dir (if (f-dir? (f-join (projectile-project-root) "lib"))
                                    "/lib/"
                                  "/src/"))
                      (guess (s-replace-all `((".test" . "")
                                              ("/test/" . ,impl-dir))
                                            file)))
            (if (file-directory-p (f-no-ext guess))
                (f-join (f-no-ext guess) "index.js")
              guess))))

    (defun cb-projectile--substitute-impl-with-test (&optional existing)
      (or existing
          (when-let* ((file (buffer-file-name))
                      (guess (replace-regexp-in-string (rx "/" (or "lib" "src") "/") "/test/" file t t)))
            (cond
             ((file-exists-p guess)
              guess)

             ((equal "index.js" (file-name-nondirectory file))
              (let ((dir (file-name-directory (directory-file-name (file-name-directory guess))))
                    (base (file-name-base (directory-file-name (file-name-directory guess))))
                    (ext (file-name-extension guess)))
                (f-join dir (format "%s.test.%s" base ext))))

             ((equal "js" (file-name-extension guess))
              (let ((dir (file-name-directory guess))
                    (base (file-name-nondirectory (file-name-sans-extension guess)))
                    (ext (file-name-extension guess)))
                (f-join dir (format "%s.test.%s" base ext))))
             (t
              guess)))))

    (defun cb-projectile-test-project (arg)
      (interactive "P")
      (let ((compilation-buffer-name-function (-const "*projectile-test*")))
        (projectile-test-project arg))))

  :init
  (progn
    (spacemacs-keys-set-leader-keys
      "p TAB" #'projectile-toggle-between-implementation-and-test
      "p <backtab>" #'projectile-find-implementation-or-test-other-window
      "p!" #'projectile-run-shell-command-in-root
      "p&" #'projectile-run-async-shell-command-in-root
      "pI" #'projectile-invalidate-cache
      "pa" #'projectile-ag
      "pD" #'projectile-dired
      "pc" #'projectile-compile-project
      "pr" #'projectile-replace
      "pt" #'cb-projectile-test-project
      "pu" #'projectile-run-project))

  :config
  (progn
    (setq projectile-cache-file (concat cb-emacs-cache-directory "/projectile.cache"))
    (setq projectile-known-projects-file (concat cb-emacs-cache-directory "/projectile-bookmarks.eld"))

    ;; Ensure projectile's known projects list doesn't contain duplicates.
    (advice-add #'projectile-unserialize :filter-return #'cb-projectile-functions-cleanup-projects)

    (projectile-load-known-projects)

    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action (lambda ()
                                             (dired (projectile-project-p))))
    (setq projectile-enable-caching t)
    (setq projectile-create-missing-test-files t)

    (setq projectile-globally-ignored-files '("TAGS" ".DS_Store"))
    (setq projectile-globally-ignored-file-suffixes '("gz" "zip" "tar" "elc"))

    (setq projectile-ignored-project-function #'cb-projectile-functions-ignored-subdir-p)

    (setq projectile-globally-ignored-directories
          '(".bzr"
            ".ensime_cache"
            ".eunit"
            ".fslckout"
            ".g8"
            ".git"
            ".hg"
            ".idea"
            ".stack-work"
            ".svn"
            "dist"
            "node_modules"
            "jars"
            "target"))

    (projectile-mode)

    (projectile-register-project-type 'yarn '("yarn.lock")
                                      :compile "yarn build"
                                      :test "yarn test")

    ;; Teach projectile how to resolve npm srcs and tests.
    (advice-add #'projectile-test-file-p :filter-return #'cb-projectile--file-is-child-of-test-dir)
    (advice-add #'projectile-find-matching-file :filter-return #'cb-projectile--substitute-test-with-impl)
    (advice-add #'projectile-find-matching-test :filter-return #'cb-projectile--substitute-impl-with-test)

    ;; Teach projectile to prefer rg for finding files containing strings
    (advice-add 'projectile-files-with-string :around #'cb-projectile--find-files-with-string-using-rg)))

(use-package counsel-projectile
  :straight t
  :defer t
  :commands (counsel-projectile-mode
             counsel-projectile-find-file
             counsel-projectile-find-dir
             counsel-projectile-switch-project
             counsel-projectile-switch-to-buffer
             counsel-projectile-rg)
  :preface
  (progn
    (autoload 'magit-list-repos "magit")

    (defun cb-projectile--refresh-projects ()
      (projectile-cleanup-known-projects)
      (dolist (repo (magit-list-repos))
        (projectile-add-known-project (file-name-as-directory repo)))
      (setq projectile-known-projects (cb-projectile-functions-cleanup-projects projectile-known-projects))))

  :init
  (spacemacs-keys-set-leader-keys
    "pf" #'counsel-projectile-find-file
    "pd" #'counsel-projectile-find-dir
    "pb" #'counsel-projectile-switch-to-buffer
    "/"  #'counsel-projectile-rg)

  :config
  (progn
    (advice-add #'counsel-projectile-switch-project :before #'cb-projectile--refresh-projects)
    (counsel-projectile-mode)))

(provide 'cb-projectile)

;;; cb-projectile.el ends here
