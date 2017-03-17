;;; cb-scala.el --- Configuration for Scala packages.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (autoload 'evil-define-key "evil-core"))

(require 'cb-emacs)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 's)
(require 'spacemacs-keys)
(require 'subr-x)

(autoload 'projectile-project-name "projectile")

;; Use conf mode for play routes files.
(add-to-list 'auto-mode-alist (cons "/routes\\'" #'conf-unix-mode))

(use-package scala-mode
  :defer t
  :mode (("\\.scala\\'" . scala-mode))
  :interpreter
  ("scala" . scala-mode)
  :config
  (progn
    (setq scala-indent:align-forms t)
    (setq scala-indent:align-parameters t)
    (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy)

    ;; KLUDGE: Don't use the auto-mode form provided by this package.
    (setq auto-mode-alist (delete '("\\.\\(scala\\|sbt\\)\\'" . scala-mode) auto-mode-alist))

    ;; KLUDGE: Scala mode exposes the face, but doesn't apply it. :/
    (font-lock-add-keywords 'scala-mode
                    `((,(rx symbol-start "var" symbol-end) 0 'scala-font-lock:var-keyword-face)))))

(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition 'minibuffer-complete-word
                             'self-insert-command
                             minibuffer-local-completion-map))

(eval-when-compile
  (defconst cb-scala-ensime-load-path (concat cb-emacs-lisp-directory "/ensime-emacs")))

(use-package ensime
  :load-path cb-scala-ensime-load-path
  :defer t
  :commands (ensime)

  ;; Define a command to send the current file to the scala repl.

  :preface
  (defun cb-scala-send-file (file)
    "Quickly load current file in the Ensime repl."
    (interactive (list (buffer-file-name)))
    (save-buffer)
    (ensime-inf-load-file file))
  :config
  (dolist (state '(normal insert))
    (eval `(evil-define-key ',state ensime-mode-map (kbd "C-c C-l") 'cb-scala-send-file)))

  :config
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'scala-mode
      "/" 'ensime-search
      "'" 'ensime-inf-switch
      "bc" 'ensime-sbt-do-compile
      "bC" 'ensime-sbt-do-clean
      "bi" 'ensime-sbt-switch
      "bp" 'ensime-sbt-do-package
      "br" 'ensime-sbt-do-run
      "ct" 'ensime-typecheck-current-buffer
      "cT" 'ensime-typecheck-all
      "ee" 'ensime-print-errors-at-point
      "el" 'ensime-show-all-errors-and-warnings
      "es" 'ensime-stacktrace-switch
      "gg" 'ensime-edit-definition
      "gp" 'ensime-pop-find-definition-stack
      "gi" 'ensime-goto-impl
      "gt" 'ensime-goto-test
      "hh" 'ensime-show-doc-for-symbol-at-point
      "hT" 'ensime-type-at-point-full-name
      "ht" 'ensime-type-at-point
      "hu" 'ensime-show-uses-of-symbol-at-point
      "ii" 'ensime-import-type-at-point
      "iI" 'ensime-inspect-type-at-point-other-frame
      "ip" 'ensime-inspect-project-package
      "nF" 'ensime-reload-open-files
      "ns" 'ensime
      "ra" 'ensime-refactor-add-type-annotation
      "rd" 'ensime-refactor-diff-inline-local
      "rD" 'ensime-undo-peek
      "rf" 'ensime-format-source
      "ri" 'ensime-refactor-diff-organize-imports
      "rm" 'ensime-refactor-diff-extract-method
      "rr" 'ensime-refactor-diff-rename
      "rt" 'ensime-import-type-at-point
      "rv" 'ensime-refactor-diff-extract-local
      "ta" 'ensime-sbt-do-test-dwim
      "tr" 'ensime-sbt-do-test-quick-dwim
      "tt" 'ensime-sbt-do-test-only-dwim
      "sa" 'ensime-inf-load-file
      "sb" 'ensime-inf-eval-buffer
      "si" 'ensime-inf-switch
      "sr" 'ensime-inf-eval-region)

    (dolist (state '(normal insert))
      (eval `(evil-define-key ',state ensime-mode-map
               (kbd "M-.") 'ensime-edit-definition
               (kbd "M-,") 'ensime-pop-find-definition-stack
               (kbd "C-c C-z") 'ensime-inf-switch)))

    (evil-define-key 'normal ensime-popup-buffer-map
      (kbd "q") 'ensime-popup-buffer-quit-function)

    (evil-define-key 'normal ensime-mode-map
      (kbd "K") 'ensime-inspect-type-at-point)

    (evil-define-key 'normal ensime-inspector-mode-map
      (kbd "M-.") 'ensime-inspector-browse-source
      (kbd "K") 'ensime-inspector-browse-doc
      (kbd "q") 'ensime-popup-buffer-quit-function
      (kbd ",") 'ensime-inspector-backward-page
      (kbd ".") 'ensime-inspector-forward-page
      (kbd "^") 'ensime-inspector-backward-page)


    (evil-define-key 'normal ensime-refactor-info-map
      (kbd "q") 'cb-scala/ensime-refactor-cancel
      (kbd "c") 'cb-scala/ensime-refactor-accept
      (kbd "RET") 'cb-scala/ensime-refactor-accept)

    (evil-define-key 'normal ensime-compile-result-map
      (kbd "g") 'ensime-show-all-errors-and-warnings
      (kbd "TAB") 'forward-button
      (kbd "<backtab>") 'backward-button
      (kbd "M-n") 'forward-button
      (kbd "M-p") 'backward-button
      (kbd "n") 'forward-button
      (kbd "N") 'backward-button))

  :init
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'scala-mode
      "ns" #'ensime)

    (dolist (prefix '(("mb" . "build")
                      ("mc" . "check")
                      ("md" . "debug")
                      ("me" . "errors")
                      ("mg" . "goto")
                      ("mh" . "docs")
                      ("mi" . "inspect")
                      ("mn" . "ensime")
                      ("mr" . "refactor")
                      ("mt" . "test")
                      ("ms" . "repl")
                      ("my" . "yank")))
      (spacemacs-keys-declare-prefix-for-mode 'scala-mode (car prefix) (cdr prefix))))

  :preface
  (progn
    (defun cb-scala--delete-existing-ensime-process-buffer (&rest _)
      (let ((bufname (format "*ENSIME-%s*" (projectile-project-name))))
        (when (get-buffer bufname)
          (kill-buffer bufname))))

    (defun cb-scala--display-ensime-process-buffer-on-error (&rest _)
      (let* ((bufname (format "*ENSIME-%s*" (projectile-project-name)))
             (buf (get-buffer-create bufname)))
        (when-let (proc (get-buffer-process buf))
          (set-process-sentinel proc
                                (lambda (p _)
                                  (when (and (/= 0 (process-exit-status p))
                                             (buffer-live-p buf))
                                    (display-buffer buf)))))))

    ;; Use compilation mode for ensime config generation buffers.

    (defconst cb-scala--ensime-gen-config-buffer "*ensime-gen-config*")

    (defun cb-scala--refresh-config-sbt (project-root task on-success-fn)
      (let ((default-directory (file-name-as-directory project-root))
            (compilation-buffer-name-function (lambda (_) cb-scala--ensime-gen-config-buffer)))
        (unless (executable-find ensime-sbt-command) (error "SBT command not found"))

        (compile (format "%s %s" ensime-sbt-command task))

        (with-current-buffer (get-buffer cb-scala--ensime-gen-config-buffer)
          (display-buffer (current-buffer))
          (set-process-sentinel (get-buffer-process (current-buffer))
                                (lambda (process event)
                                  (ensime--refresh-config-sentinel process event on-success-fn))))
        (message "Updating ENSIME config..."))))

  :config
  (progn
    (setq ensime-startup-snapshot-notification nil)
    (setq ensime-auto-generate-config t)
    (setq ensime-implicit-gutter-icons nil)
    (setq ensime-sem-high-enabled-p nil)

    (advice-add 'ensime :before #'cb-scala--delete-existing-ensime-process-buffer)
    (advice-add 'ensime :after #'cb-scala--display-ensime-process-buffer-on-error)

    (defalias 'ensime--refresh-config-sbt #'cb-scala--refresh-config-sbt)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*ENSIME-" (+? nonl) "*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*ensime-gen-config*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))))

(use-package ensime-company
  :after ensime
  :preface
  (progn
    (autoload 'company-mode "company")
    (autoload 'yas-minor-mode-on "yasnippet"))

  :config
  ;; HACK: Prevent ensime from clobbering company settings.
  (with-eval-after-load 'ensime-company
    (defun ensime-company-enable ()
      (set (make-local-variable 'company-backends) '(ensime-company))
      (company-mode)
      (yas-minor-mode-on)
      (set (make-local-variable 'company-idle-delay) 0))))

(use-package cb-ensime-test-config
  :after ensime
  :config
  (setq ensime-goto-test-config-defaults cb-ensime-test-config-defaults))

(use-package cb-scala-sbt-file-mode
  :mode ("\\.sbt\\'". cb-scala-sbt-file-mode))

(use-package flycheck
  :defer t
  :preface
  (defun cb-scala--disable-flycheck-scala ()
    (when (boundp 'flycheck-disabled-checkers)
      (push 'scala flycheck-disabled-checkers)))

  :config
  (progn
    (add-hook 'cb-scala-sbt-file-mode-hook #'cb-scala--disable-flycheck-scala)
    (add-hook 'ensime-mode-hook #'cb-scala--disable-flycheck-scala)))

(use-package ensime-flycheck-integration
  :after ensime
  :commands (ensime-flycheck-integration-next-error
             ensime-flycheck-integration-prev-error)
  :config
  (progn
    (define-key ensime-mode-map (kbd "M-n") #'ensime-flycheck-integration-next-error)
    (define-key ensime-mode-map (kbd "M-p") #'ensime-flycheck-integration-prev-error)
    (define-key ensime-mode-map (kbd "M-N") #'ensime-flycheck-integration-next-error)
    (define-key ensime-mode-map (kbd "M-P") #'ensime-flycheck-integration-prev-error)
    (evil-define-key 'normal ensime-mode-map (kbd "M-n") #'ensime-flycheck-integration-next-error)
    (evil-define-key 'normal ensime-mode-map (kbd "M-p") #'ensime-flycheck-integration-prev-error)
    (evil-define-key 'normal ensime-mode-map (kbd "M-N") #'ensime-flycheck-integration-next-error)
    (evil-define-key 'normal ensime-mode-map (kbd "M-P") #'ensime-flycheck-integration-prev-error)))

(use-package aggressive-indent
  :defer t
  :config
  (progn
    (add-to-list 'aggressive-indent-excluded-modes 'scala-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'cb-scala-sbt-file-mode)))

(use-package cb-scala-autoinsert
  :after autoinsert
  :preface (autoload 'cb-scala-autoinsert-init "cb-scala-autoinsert")
  :config (cb-scala-autoinsert-init))

;; Snippet utilities


;; Slick table mapping template

(defun cb-scala-yasnippet-slick-star-fields (attrs)
  (let ((names (--map (plist-get it :name) (cb-scala-yasnippet--parse-attrs attrs))))
    (s-join ", " names)))

(defun cb-scala-yasnippet-slick-column-defs (attrs)
  (let ((defs (-map 'scala-yasnippet--slick-attr-to-def (cb-scala-yasnippet--parse-attrs attrs)))
        (indent (current-indentation)))
    (s-join (concat "\n" (s-repeat indent " ")) defs)))

(defun cb-scala-yasnippet--slick-attr-to-def (attr)
  (-let [(&plist :name name :type type) attr]
    (format "def %s = column[%s](\"%s\")" name type name)))

(defun cb-scala-yasnippet--parse-attrs (attrs)
  (let ((ctor-args (s-split (rx (* space) "," (* space)) attrs)))
    (--map (-let [(_ name type) (s-match (rx (group (*? nonl))
                                             (* space) ":" (* space)
                                             (group (* nonl)))
                                         (s-trim it))]
             (list :name (or name "x") :type (or type "T")))
           ctor-args)))

;; Test fixtures

(defun cb-scala-yasnippet-test-fixture-name ()
  (or (ignore-errors (f-filename (f-no-ext (buffer-file-name))))
      "TestFixture"))


(provide 'cb-scala)

;;; cb-scala.el ends here
