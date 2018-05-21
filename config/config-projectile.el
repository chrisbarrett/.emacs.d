;;; config-projectile.el --- Configuration for projectile.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'subr-x))

(require 'dash-functional)
(require 'paths)
(require 'projectile-funcs)

(autoload 'magit-status-internal "magit")

(use-package projectile-funcs
  :defines (projectile-funcs-ignored-base-dirs)
  :config
  (setq projectile-funcs-ignored-base-dirs
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
    (autoload 'magit-status-internal "magit")
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

  :config
  (progn
    (advice-add 'projectile-load-known-projects :override #'projectile-funcs-refresh-projects)
    (advice-add 'projectile-save-known-projects :override #'ignore)

    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action #'magit-status-internal)
    (setq projectile-enable-caching t)
    (setq projectile-create-missing-test-files t)

    (setq projectile-globally-ignored-files '("TAGS" ".DS_Store"))
    (setq projectile-globally-ignored-file-suffixes '("gz" "zip" "tar" "elc"))

    (setq projectile-ignored-project-function #'projectile-funcs-ignored-subdir-p)

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
  :config
  (progn
    (setq counsel-projectile-switch-project-action #'magit-status-internal)
    (counsel-projectile-mode)))

(provide 'config-projectile)

;;; config-projectile.el ends here
