;;; config-python.el --- Configuration for python.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'paths)
(require 'spacemacs-keys)
(require 'evil)
(autoload 'xref-push-marker-stack "xref")

(use-package python
  :defer t
  :preface
  (progn
    (autoload 'python-indent-dedent-line "python")
    (autoload 'python-shell-get-process "python")
    (autoload 'sp-backward-delete-char "smartparens")

    (defun cb-python--init-python-mode ()
      (setq-local comment-inline-offset 2)
      (setq-local tab-width 4)
      (setq-local evil-shift-width 4)
      (prettify-symbols-mode -1)
      (when (executable-find "ipython")
        (setq-local python-shell-interpreter "ipython")
        (setq-local python-shell-interpreter-args "--simple-prompt -i")))

    (defun cb-python-backspace ()
      (interactive)
      (if (equal (char-before) ?\s)
          (unless (python-indent-dedent-line)
            (backward-delete-char-untabify 1))
        (sp-backward-delete-char)))

    (defvar cb-python-prev-source-buffer)

    (defun cb-python-repl-switch-to-source ()
      (interactive)
      (-when-let (buf cb-python-prev-source-buffer)
        (when (buffer-live-p buf)
          (pop-to-buffer buf))))

    (defun cb-python-repl ()
      "Start and/or switch to the REPL."
      (interactive)
      (when (derived-mode-p 'python-mode)
        (setq cb-python-prev-source-buffer (current-buffer)))
      (let ((shell-process
             (or (python-shell-get-process)
                 (with-demoted-errors "Error: %S"
                   (call-interactively #'run-python)
                   (python-shell-get-process)))))
        (unless shell-process
          (error "Failed to start python shell properly"))
        (pop-to-buffer (process-buffer shell-process))
        (evil-insert-state))))

  :init
  (add-hook 'python-mode-hook #'cb-python--init-python-mode)

  :config
  (progn
    (setq python-indent-guess-indent-offset nil)
    (setq python-indent-offset 4)
    (setq python-fill-docstring-style 'django)

    (push "jupyter" python-shell-completion-native-disabled-interpreters)

    (define-key python-mode-map [remap python-indent-dedent-line-backspace]  #'cb-python-backspace)
    (define-key python-mode-map [remap python-shell-switch-to-shell] #'cb-python-repl)
    (define-key inferior-python-mode-map (kbd "C-c C-z") #'cb-python-repl-switch-to-source)

    (spacemacs-keys-declare-prefix-for-mode 'python-mode "ms" "shell")
    (spacemacs-keys-set-leader-keys-for-major-mode 'python-mode
      "eb" 'python-shell-send-buffer
      "sb" 'python-shell-send-buffer
      "sf" 'python-shell-send-file)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Python*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (slot            . 0)
                   (window-height   . 0.2)))))

(use-package which-key
  :config
  (progn
    (push `((nil . ,(rx bos "anaconda-mode-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `((nil . ,(rx bos "pytest-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `((nil . ,(rx bos "python-shell-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)

    (push `((nil . ,(rx bos (? "cb-python-") "pyvenv-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package flycheck
  :defer t
  :config
  (setq flycheck-python-pycompile-executable "python"))

(use-package anaconda-mode
  :straight t
  :commands (anaconda-mode)
  :preface
  (progn
    (autoload 'anaconda-mode-find-definitions "anaconda-mode")

    (defun cb-python--push-mark (&rest _)
      (xref-push-marker-stack)))

  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  :config
  (progn
    (let ((dir (f-join paths-cache-directory "anaconda-mode")))
      (f-mkdir dir)
      (setq anaconda-mode-installation-directory dir))


    ;; Main keybindings

    (spacemacs-keys-set-leader-keys-for-major-mode 'python-mode
      "a" 'anaconda-mode-find-assignments
      "r" 'anaconda-mode-find-references)

    (evil-define-key 'normal anaconda-mode-map (kbd "K") #'anaconda-mode-show-doc)
    (evil-define-key 'normal anaconda-mode-map (kbd "M-.") #'anaconda-mode-find-definitions)
    (evil-define-key 'normal anaconda-mode-map (kbd "M-,") #'pop-tag-mark)
    (define-key anaconda-mode-map (kbd "M-.") #'anaconda-mode-find-definitions)
    (define-key anaconda-mode-map (kbd "M-,") #'pop-tag-mark)

    (evil-set-initial-state 'anaconda-mode-view-mode 'motion)
    (evil-define-key 'motion anaconda-mode-view-mode-map (kbd "q") 'quit-window)

    ;; Advice

    (advice-add 'anaconda-mode-find-assignments :before #'cb-python--push-mark)
    (advice-add 'anaconda-mode-find-definitions :before #'cb-python--push-mark)))

(use-package company-anaconda
  :straight t
  :defer t
  :preface
  (defun cb-python--enable-company-anaconda ()
    (with-no-warnings
      (add-to-list 'company-backends 'company-anaconda)))
  :config
  (add-hook 'anaconda-mode-hook #'cb-python--enable-company-anaconda))

(use-package pytest
  :straight t
  :after python
  :commands (pytest-one
             pytest-pdb-one
             pytest-all
             pytest-pdb-all
             pytest-module
             pytest-pdb-module
             pytest-suite
             pytest-pdb-suite)
  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'python-mode "mt" "test")
    (spacemacs-keys-set-leader-keys-for-major-mode 'python-mode
      "tA" #'pytest-pdb-all
      "ta" #'pytest-all
      "tT" #'pytest-pdb-one
      "tt" #'pytest-one
      "tM" #'pytest-pdb-module
      "tm" #'pytest-module
      "tS" #'pytest-pdb-suite
      "ts" #'pytest-suite))
  :config
  (add-to-list 'pytest-project-root-files "setup.cfg"))

(use-package pip-requirements
  :straight t
  :mode (("\\.pip\\'" . pip-requirements-mode)
         ("requirements.+\\.txt\\'" . pip-requirements-mode)
         ("requirements\\.in\\'" . pip-requirements-mode)))

(use-package pyvenv
  :straight t
  :commands (pyvenv-activate pyvenv-deactivate pyvenv-workon)
  :preface
  (progn
    (autoload 'projectile-project-p "projectile")
    (autoload 'f-join "f")

    (defvar cb-python--venv-names '(".env" "env" ".venv" "venv" ".virtualenv"))

    (defun cb-python--directory-first-ancestor (dir pred)
      "Search up the filesystem for the first DIR satisfying PRED.
Return the first non-nil result of evalutating PRED."
      (let (result)
        (while dir
          (pcase (funcall pred dir)
            (`nil
             (setq dir (f-parent dir)))
            (res
             (setq result res)
             (setq dir nil))))
        result))

    (defun cb-python--find-venv-in-directory (dir)
      (-when-let ((dir) (--keep (let ((dir (f-join dir it)))
                                  (when (f-directory? dir)
                                    dir))
                                cb-python--venv-names))
        (file-truename dir)))

    (defun cb-python-pyvenv-dir ()
      (cb-python--directory-first-ancestor default-directory
                                  #'cb-python--find-venv-in-directory))

    (defun cb-python-pyvenv-activate-if-found ()
      (-when-let (env (cb-python-pyvenv-dir))
        (pyvenv-activate env)
        (message "Using pyvenv at %s" (f-abbrev env))))

    (defun cb-python-pyvenv-init (env)
      (interactive
       (list (or (cb-python-pyvenv-dir)
                 (f-join (read-directory-name "Project root: " nil nil t) ".env"))))
      (when (f-dir? env)
        (user-error "Environment already exists"))
      (let ((reporter (make-progress-reporter "Initializing pyvenv environment...")))
        (pcase (call-process "pyvenv" nil nil nil env)
          (`0
           (progress-reporter-update reporter)
           (pyvenv-activate env)
           (progress-reporter-done reporter))
          (_
           (message "%sFAILED" (aref (cdr reporter) 3)))))))
  :init
  (progn
    (add-hook 'python-mode-hook #'cb-python-pyvenv-activate-if-found)
    (spacemacs-keys-declare-prefix-for-mode 'python-mode "me" "pyvenv")
    (spacemacs-keys-set-leader-keys-for-major-mode 'python-mode
      "ei" #'cb-python-pyvenv-init
      "ea" #'pyvenv-activate
      "ed" #'pyvenv-deactivate
      "ew" #'pyvenv-workon)))

;; pip install isort

(use-package py-isort
  :straight t
  :defer t
  :after 'python
  :preface
  (progn
    (defvar python-sort-imports t)

    (defun python-sort-imports-maybe ()
      (when python-sort-imports
        (py-isort-before-save)))

    (define-minor-mode python-sort-imports-mode
      "Minor mode for sorting python imports on save."
      nil nil nil nil
      (if python-sort-imports-mode
          (add-hook 'before-save-hook 'python-sort-imports-maybe nil t)
        (remove-hook 'before-save-hook 'python-sort-imports-maybe t))))
  :init
  (add-hook 'python-mode-hook #'python-sort-imports-mode))

;; pip install yapf

(use-package py-yapf
  :straight t
  :defer t
  :after 'python
  :preface
  (progn
    (defvar python-auto-format-buffer t)

    (defun python-auto-format-maybe ()
      (when python-auto-format-buffer
        (py-yapf-buffer)))

    (define-minor-mode python-auto-format-mode
      "Minor mode for sorting formatting the buffer on save."
      nil nil nil nil
      (if python-auto-format-mode
          (add-hook 'before-save-hook 'python-auto-format-maybe nil t)
        (remove-hook 'before-save-hook 'python-auto-format-maybe t))))
  :init
  (add-hook 'python-mode-hook #'python-auto-format-mode))

(provide 'config-python)

;;; config-python.el ends here
