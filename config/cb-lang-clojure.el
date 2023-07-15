;;; cb-lang-clojure.el --- Clojure configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cb-macs)

(use-package clojure-mode :ensure t)

(use-package cider :ensure t
  :general
  (:states '(normal insert) :keymaps 'cider-mode-map
   "M-." 'cider-find-dwim)
  :custom
  (cider-repl-display-help-banner nil)
  (cider-allow-jack-in-without-project t))

(use-package smartparens :ensure t
  :general
  ;; Fix mysteriously shadowed binding
  (:keymaps '(clojure-mode-map cider-repl-mode-map) :states 'insert
   "DEL" 'sp-backward-delete-char))

(mode-leader-set-key :keymaps 'clojure-mode-map
  "j" '(cider-jack-in :wk "jack in")
  "M" '(cider-run :wk "run main")
  "a" '(cider-apropos :wk "apropos")
  ":" '(cider-read-and-eval :wk "eval")

  "e" '(nil :wk "eval...")
  "es" '(cider-eval-sexp-at-point :wk "eval sexp at pt")
  "eS" '(cider-eval-sexp-at-point-in-context :wk "eval sexp at pt (bindings)")
  "eb" '(cider-eval-buffer :wk "eval buffer")
  "ed" '(cider-eval-defun-at-point :wk "eval defun")
  "en" '(cider-eval-ns-form :wk "eval ns form")
  "ep" '(cider-eval-print-last-sexp :wk "eval and print to buffer")

  "f" '(nil :wk "find...")
  "ff" '(cider-find-dwim :wk "find dwim")
  "fF" '(cider-find-dwim-other-window :wk "find dwim (other window)")
  "fk" '(cider-find-keyword :wk "find keyword")
  "fn" '(cider-find-ns :wk "find ns")
  "fr" '(cider-find-resource :wk "find resource")
  "fv" '(cider-find-var :wk "find var")

  "p" '(cider-classpath :wk "list classpath")

  "R" '(cider-restart :wk "restart cider")
  "Q" '(cider-quit :wk "quit cider")

  "s" '(cider-scratch :wk "scratch buffer")

  "t" '(nil :wk "test...")
  "ta" '(cider-auto-test-mode :wk "toggle auto-test ns")
  "tf" '(cider-test-rerun-failed-tests :wk "test failed")
  "tl" '(cider-test-run-loaded-tests :wk "test loaded")
  "tp" '(cider-test-run-project-tests :wk "test project")
  "tr" '(cider-test-rerun-test :wk "re-run last")
  "tt" '(cider-test-run-test :wk "test at point"))

(provide 'cb-lang-clojure)

;;; cb-lang-clojure.el ends here
