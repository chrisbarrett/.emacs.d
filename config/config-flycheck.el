;;; config-flycheck.el --- Flycheck configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'config-etags)
(require 'dash)
(require 'subr-x)
(require 'flycheck)



(use-package flycheck
  :hook ((after-init . global-flycheck-mode)
         (prog-mode . flycheck-mode-on-safe))

  :commands (flycheck-list-errors
             flycheck-error-list-next-error
             flycheck-error-list-previous-error
             flycheck-error-list-goto-error)

  :general
  (:keymaps
   'flycheck-mode-map
   "M-n" #'flycheck-next-error
   "M-p" #'flycheck-previous-error
   "M-j" #'flycheck-next-error
   "M-k" #'flycheck-previous-error)

  :general
  (:states
   'motion
   :keymaps 'flycheck-error-list-mode-map
   "j" #'flycheck-error-list-next-error
   "k" #'flycheck-error-list-previous-error
   "RET" #'flycheck-error-list-goto-error
   "n" #'flycheck-error-list-next-error
   "p" #'flycheck-error-list-previous-error
   "q" #'quit-window)

  :preface
  (progn
    (autoload 'flycheck-buffer "flycheck")
    (autoload 'flycheck-error-format-message-and-id "flycheck")
    (autoload 'flycheck-get-error-list-window "flycheck")
    (autoload 'flycheck-may-use-echo-area-p "flycheck")
    (autoload 'projectile-project-p "projectile")
    (autoload 'projectile-process-current-project-buffers "projectile")

    (defun config-flycheck-toggle-error-list ()
      "Show or hide the error list."
      (interactive)
      (if-let* ((window (--first (equal flycheck-error-list-buffer
                                        (buffer-name (window-buffer it)))
                                 (window-list))))
          (delete-window window)
        (flycheck-list-errors)))

    (defun config-flycheck--maybe-inhibit-flycheck (result)
      (unless (or (equal (buffer-name) "*ediff-merge*")
                  (string-match-p (rx "/node_modules/") default-directory)
                  config-etags-in-query-replace-session-p)
        result))

    (defun config-flycheck--check-all-project-buffers ()
      (unless config-etags-in-query-replace-session-p
        (when (and (bound-and-true-p projectile-mode)
                   (projectile-project-p))
          (projectile-process-current-project-buffers
           (lambda (buf)
             (with-current-buffer buf
               (when (bound-and-true-p flycheck-mode)
                 ;; HACK: Inhibit checks for elisp, otherwise flycheck will
                 ;; spawn a bunch of thrashing Emacs processes.
                 (unless (derived-mode-p 'emacs-lisp-mode)
                   (ignore-errors
                     (flycheck-buffer))))))))))

    (defun config-flycheck-display-error-messages (errors)
      (unless (flycheck-get-error-list-window 'current-frame)
        (when (and errors (flycheck-may-use-echo-area-p))
          (let ((messages (seq-map #'flycheck-error-format-message-and-id errors)))
            (display-message-or-buffer (string-join messages "\n\n")
                                       flycheck-error-message-buffer
                                       'display-buffer-popup-window))))))

  :config
  (progn
    (advice-add #'flycheck-may-enable-mode :filter-return #'config-flycheck--maybe-inhibit-flycheck)

    (add-hook 'after-save-hook #'config-flycheck--check-all-project-buffers)

    (setq flycheck-display-errors-function 'config-flycheck-display-error-messages)
    (setq flycheck-display-errors-delay 0.1)
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-python-pycompile-executable "python")

    (setq flycheck-global-modes
          '(not idris-repl-mode
                dir-locals-mode
                text-mode
                org-mode
                org-agenda-mode
                indium-repl-mode
                vterm-mode
                ;; restclient buffers
                js-mode))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 1)
                   (window-height   . 0.2)))))



(defun config-flycheck--elisp-package-flycheck-predicate ()
  (not (or (bound-and-true-p no-byte-compile)
           (bound-and-true-p org-src-mode)
           (string-match-p (rx bol "*Pp ") (buffer-name))
           (ignore-errors
             (string-match-p "^test-" (file-name-nondirectory (buffer-file-name)))))))

;; flycheck-package is a linter for Elisp package conventions.

(use-package flycheck-package
  :after (:all flycheck elisp-mode)
  :config
  (eval
   '(progn
      (flycheck-package-setup)
      (setf (flycheck-checker-get 'emacs-lisp-package 'predicate)
            #'config-flycheck--elisp-package-flycheck-predicate))))

;; Checkdoc is used by flycheck for linting docstrings in elisp.

(use-package checkdoc
  :after flycheck
  :custom
  ((checkdoc-force-docstrings-flag nil)
   (checkdoc-arguments-in-order-flag nil))
  :config
  (eval
   '(setf (flycheck-checker-get 'emacs-lisp-checkdoc 'predicate)
          #'config-flycheck--elisp-package-flycheck-predicate)))

(provide 'config-flycheck)

;;; config-flycheck.el ends here
