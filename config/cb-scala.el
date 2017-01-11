;;; cb-scala.el --- Configuration for Scala packages.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (autoload 'evil-define-key "evil-core"))

(require 'cb-emacs)
(require 'spacemacs-keys)
(require 's)
(require 'f)
(require 'dash)

(use-package scala-mode
  :defer t
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'" . sbt-file-mode))
  :interpreter
  ("scala" . scala-mode)
  :config
  (progn
    (define-derived-mode sbt-file-mode scala-mode "Scala(SBT)"
      "Major mode for editing SBT files.\n\n \\{sbt-file-mode-map}")

    (setq scala-indent:align-forms t)
    (setq scala-indent:align-parameters t)
    (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy)))

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
  :config
  (progn
    (spacemacs-keys-set-leader-keys-for-major-mode 'scala-mode
      (kbd "/") 'ensime-search
      (kbd "'") 'ensime-inf-switch
      (kbd "bc") 'ensime-sbt-do-compile
      (kbd "bC") 'ensime-sbt-do-clean
      (kbd "bi") 'ensime-sbt-switch
      (kbd "bp") 'ensime-sbt-do-package
      (kbd "br") 'ensime-sbt-do-run
      (kbd "ct") 'ensime-typecheck-current-buffer
      (kbd "cT") 'ensime-typecheck-all
      (kbd "ee") 'ensime-print-errors-at-point
      (kbd "el") 'ensime-show-all-errors-and-warnings
      (kbd "es") 'ensime-stacktrace-switch
      (kbd "gg") 'ensime-edit-definition
      (kbd "gp") 'ensime-pop-find-definition-stack
      (kbd "gi") 'ensime-goto-impl
      (kbd "gt") 'ensime-goto-test
      (kbd "hh") 'ensime-show-doc-for-symbol-at-point
      (kbd "hT") 'ensime-type-at-point-full-name
      (kbd "ht") 'ensime-type-at-point
      (kbd "hu") 'ensime-show-uses-of-symbol-at-point
      (kbd "ii") 'ensime-import-type-at-point
      (kbd "iI") 'ensime-inspect-type-at-point-other-frame
      (kbd "ip") 'ensime-inspect-project-package
      (kbd "nF") 'ensime-reload-open-files
      (kbd "ns") 'ensime
      (kbd "ra") 'ensime-refactor-add-type-annotation
      (kbd "rd") 'ensime-refactor-diff-inline-local
      (kbd "rD") 'ensime-undo-peek
      (kbd "rf") 'ensime-format-source
      (kbd "ri") 'ensime-refactor-diff-organize-imports
      (kbd "rm") 'ensime-refactor-diff-extract-method
      (kbd "rr") 'ensime-refactor-diff-rename
      (kbd "rt") 'ensime-import-type-at-point
      (kbd "rv") 'ensime-refactor-diff-extract-local
      (kbd "ta") 'ensime-sbt-do-test-dwim
      (kbd "tr") 'ensime-sbt-do-test-quick-dwim
      (kbd "tt") 'ensime-sbt-do-test-only-dwim
      (kbd "sa") 'ensime-inf-load-file
      (kbd "sb") 'ensime-inf-eval-buffer
      (kbd "si") 'ensime-inf-switch
      (kbd "sr") 'ensime-inf-eval-region
      (kbd "z") 'ensime-expand-selection-command)

    (dolist (state '(normal insert))
      (eval `(evil-define-key ',state ensime-mode-map
               (kbd "M-.") 'ensime-edit-definition
               (kbd "M-,") 'ensime-pop-find-definition-stack)))
    
    (evil-define-key 'normal ensime-popup-buffer-map
      (kbd "q") 'ensime-popup-buffer-quit-function)

    (evil-define-key ensime-inspector-mode-map
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

  :config
  (progn
    (setq ensime-startup-snapshot-notification nil)
    (setq ensime-auto-generate-config t)
    (setq ensime-implicit-gutter-icons nil)
    (setq ensime-sem-high-enabled-p nil)))

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


(use-package flycheck
  :defer t
  :preface
  (defun cb-scala--disable-flycheck-scala ()
    (when (boundp 'flycheck-disabled-checkers)
      (push 'scala flycheck-disabled-checkers)))

  :config
  (progn
    (setq flycheck-scalastylerc "~/.scalastyle.xml")
    (add-hook 'ensime-mode-hook #'cb-scala--disable-flycheck-scala)) )

(use-package ensime-flycheck-integration
  :after ensime
  :commands (ensime-flycheck-integration-next-error
             ensime-flycheck-integration-prev-error)
  :config
  (progn
    (define-key ensime-mode-map (kbd "M-N") #'ensime-flycheck-integration-next-error)
    (define-key ensime-mode-map (kbd "M-P") #'ensime-flycheck-integration-prev-error)
    (evil-define-key 'normal ensime-mode-map (kbd "M-N") #'ensime-flycheck-integration-next-error)
    (evil-define-key 'normal ensime-mode-map (kbd "M-P") #'ensime-flycheck-integration-prev-error)))

(use-package aggressive-indent
  :defer t
  :config
  (progn
    (add-to-list 'aggressive-indent-excluded-modes 'scala-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'sbt-file-mode)))

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
