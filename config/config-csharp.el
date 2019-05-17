;;; config-csharp.el --- Configuration for C#.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))



;; csharp-mode provides the major-mode for the C# language.

(use-package csharp-mode
  :straight t
  :mode ("\\.cs\\'" . csharp-mode)
  :preface
  (defun config-csharp--find-solution ()
    (f-traverse-upwards
     (lambda (dir)
       (f-files dir
                (lambda (it) (f-ext-p it "sln"))))
     default-directory))
  :init
  (with-eval-after-load 'projectile
    (projectile-register-project-type 'dotnet #'config-csharp--find-solution
                                      :compile "msbuild"
                                      :test "nunit-console")))

;; Omnisharp provides IDE-like features for C#.

(use-package omnisharp
  :straight t
  :defer t
  :preface
  (defun config-csharp--set-up-omnisharp-buffer ()
    (evil-local-set-key 'insert (kbd ".") #'omnisharp-add-dot-and-auto-complete)
    (setq-local evil-lookup-func #'omnisharp-current-type-documentation))

  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-omnisharp))
  :hook ((csharp-mode . omnisharp-mode)
         (omnisharp-mode . eldoc-mode)
         (omnisharp-mode . config-csharp--set-up-omnisharp-buffer))
  :general
  (:states '(insert normal) :keymaps 'omnisharp-mode-map
   "M-." 'omnisharp-go-to-definition))

(provide 'config-csharp)

;;; config-csharp.el ends here
