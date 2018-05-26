;;; config-projectile.el --- Configuration for projectile.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'subr-x))

(require 'dash-functional)
(require 'paths)
(require 'projectile-funcs)



;; projectile-funcs contains functions used by the configuration for projectile.

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

;; Projectile provides commands for working with projects.

(use-package projectile
  :straight t
  :commands (projectile-mode)
  :preface
  (progn
    (autoload 'projectile-files-from-cmd "projectile")
    (autoload 'projectile-find-matching-file "projectile")
    (autoload 'projectile-find-matching-test "projectile")
    (autoload 'projectile-project-root "projectile")
    (autoload 'projectile-register-project-type "projectile")
    (autoload 'projectile-test-file-p "projectile")
    (autoload 'projectile-test-project "projectile")
    (autoload 'projectile-unixy-system-p "projectile")

    (defun config-projectile--find-files-with-string-using-rg (fn string directory)
      (if (and (projectile-unixy-system-p) (executable-find "rg"))
          (let* ((search-term (shell-quote-argument string))
                 (cmd (concat "rg --fixed-strings --color=never --no-heading --files-with-matches -- " search-term)))

            (projectile-files-from-cmd cmd directory))
        (funcall fn string directory)))

    (defun config-projectile--file-is-child-of-test-dir (&optional has-test-prefix-or-suffix)
      (or has-test-prefix-or-suffix
          (when-let* ((file (buffer-file-name)))
            (seq-contains (f-split file) "test"))))

    (defun config-projectile--substitute-test-with-impl (&optional existing)
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

    (defun config-projectile--substitute-impl-with-test (&optional existing)
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

    (defun config-projectile-test-project (arg)
      (interactive "P")
      (let ((compilation-buffer-name-function (-const "*projectile-test*")))
        (projectile-test-project arg))))

  :config
  (progn
    (advice-add 'projectile-load-known-projects :override #'projectile-funcs-refresh-projects)
    (advice-add 'projectile-save-known-projects :override #'ignore)

    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action #'dired)
    (setq projectile-enable-caching t)
    (setq projectile-create-missing-test-files t)

    (setq projectile-globally-ignored-files '("TAGS" ".DS_Store"))
    (setq projectile-globally-ignored-file-suffixes '("gz" "zip" "tar" "elc"))

    (setq projectile-ignored-project-function #'projectile-funcs-ignored-subdir-p)

    (setq projectile-globally-ignored-directories
          '(
            ".bzr"
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
            "jars"
            "node_modules"
            "straight/repos"
            "target"
            ))

    (projectile-mode)

    (projectile-register-project-type 'yarn '("yarn.lock")
                                      :compile "yarn build"
                                      :test "yarn test")

    ;; Teach projectile how to resolve npm srcs and tests.
    (advice-add #'projectile-test-file-p :filter-return #'config-projectile--file-is-child-of-test-dir)
    (advice-add #'projectile-find-matching-file :filter-return #'config-projectile--substitute-test-with-impl)
    (advice-add #'projectile-find-matching-test :filter-return #'config-projectile--substitute-impl-with-test)

    ;; Teach projectile to prefer rg for finding files containing strings
    (advice-add 'projectile-files-with-string :around #'config-projectile--find-files-with-string-using-rg)))

;; counsel-projectile provides ivy wrappers for projectile commands.

(use-package counsel-projectile
  :straight t
  :defer t
  :commands (counsel-projectile-mode)
  :config
  (progn
    (setq counsel-projectile-switch-project-action #'dired)
    (counsel-projectile-mode)))

(provide 'config-projectile)

;;; config-projectile.el ends here
