(setq debug-on-error t
      debug-on-quit t
      ensime-log-events nil ;; change to `t' for debug info
      ensime--debug-messages nil ;; change to `t' for debug info
      ensime-typecheck-when-idle nil
      company-backends nil
      ensime-startup-snapshot-notification nil
      user-emacs-directory (expand-file-name
                            (format ".cask/%s.%s"
                                    emacs-major-version
                                    emacs-minor-version)))

;; disables the welcome message
(mkdir (expand-file-name "ensime" user-emacs-directory) 'parents)

;; can be useful when debugging the test framework
;; (add-hook 'kill-emacs-hook (lambda() (backtrace)))

;; Cask has downloaded everything for us
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

;;(add-to-list 'package-archives `("local" . ,(expand-file-name "dist/")))
(package-install-file (car (file-expand-wildcards "dist/*.tar")))

(require 'use-package)

;; enable coverage
(when (getenv "UNDERCOVER")
  (require 'undercover)
  (undercover "ensime*.el"
              (:report-file (expand-file-name "coveralls.json"))
              (:send-report nil)
              (:exclude "ensime-test.el" "dotemacs_test.el")))

(use-package ensime)

(load-file "ensime-test.el")

;; uncomment to debug the server
;;(setq ensime-server-logback (concat ensime-test-dev-home "/test/logback.xml"))

(message "Using ensime-test-dev-home of %s" ensime-test-dev-home)

;;; dotemacs_test.el ends here
