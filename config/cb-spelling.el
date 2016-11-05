;;; cb-spelling.el --- Configuration for spelling features  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'cb-use-package-extensions))

(use-package ispell
  :config
  (progn
    (setq ispell-program-name "aspell")
    (setq ispell-dictionary "british")
    (setq ispell-silently-savep t)))

(use-package cb-evil-ispell
  :commands (cb-evil-ispell-previous-spelling-error
             cb-evil-ispell-next-spelling-error
             cb-evil-ispell-mark-word-as-good
             cb-evil-ispell-mark-word-as-locally-good
             cb-evil-ispell-correct-word)
  :preface
  (autoload 'flyspell-auto-correct-word "flyspell")
  :init
  (with-eval-after-load 'evil
    (evil-global-set-key 'normal (kbd "z u") #'flyspell-auto-correct-word)
    (evil-global-set-key 'normal (kbd "[s")  #'cb-evil-ispell-previous-spelling-error)
    (evil-global-set-key 'normal (kbd "]s")  #'cb-evil-ispell-next-spelling-error)
    (evil-global-set-key 'normal (kbd "z g") #'cb-evil-ispell-mark-word-as-good)
    (evil-global-set-key 'normal (kbd "z G") #'cb-evil-ispell-mark-word-as-locally-good)
    (evil-global-set-key 'normal (kbd "z =") #'cb-evil-ispell-correct-word)))

(provide 'cb-spelling)

;;; cb-spelling.el ends here
