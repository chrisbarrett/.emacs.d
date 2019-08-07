;;; config-python.el --- Configuration for python.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'major-mode-hydra)
(require 'paths)

(major-mode-hydra-bind python-mode "Eval"
  ("eb" python-shell-send-buffer "buffer")
  ("ef" python-shell-send-file "file"))

(major-mode-hydra-bind python-mode "Find"
  ("a" anaconda-mode-find-assignments "assignments")
  ("r" anaconda-mode-find-references "references"))

(major-mode-hydra-bind python-mode "Test"
  ("ta" pytest-all "all")
  ("tt" pytest-one "one")
  ("tm" pytest-module "module")
  ("ts" pytest-suite "suite"))

(major-mode-hydra-bind python-mode "Venv"
  ("vi" config-python-pyvenv-init "init")
  ("va" pyvenv-activate "activate")
  ("vd" pyvenv-deactivate "deactivate")
  ("vw" pyvenv-workon "workon"))



(use-package python
  :defer t
  :hook (python-mode . config-python--init-python-mode)
  :preface
  (progn
    (autoload 'evil-insert-state "evil-states")
    (autoload 'python-indent-dedent-line "python")
    (autoload 'python-shell-get-process "python")
    (autoload 'sp-backward-delete-char "smartparens")

    (defun config-python--init-python-mode ()
      (setq-local comment-inline-offset 2)
      (setq-local tab-width 4)
      (setq-local evil-shift-width 4)
      (prettify-symbols-mode -1)
      (when (executable-find "ipython")
        (setq-local python-shell-interpreter "ipython")
        (setq-local python-shell-interpreter-args "--simple-prompt -i")))

    (defun config-python-backspace ()
      (interactive)
      (if (equal (char-before) ?\s)
          (unless (python-indent-dedent-line)
            (backward-delete-char-untabify 1))
        (sp-backward-delete-char)))

    (defvar config-python-prev-source-buffer)

    (defun config-python-repl-switch-to-source ()
      (interactive)
      (-when-let (buf config-python-prev-source-buffer)
        (when (buffer-live-p buf)
          (pop-to-buffer buf))))

    (defun config-python-repl ()
      "Start and/or switch to the REPL."
      (interactive)
      (when (derived-mode-p 'python-mode)
        (setq config-python-prev-source-buffer (current-buffer)))
      (let ((shell-process
             (or (python-shell-get-process)
                 (with-demoted-errors "Error: %S"
                   (call-interactively #'run-python)
                   (python-shell-get-process)))))
        (unless shell-process
          (error "Failed to start python shell properly"))
        (pop-to-buffer (process-buffer shell-process))
        (evil-insert-state))))
  :config
  (progn
    (setq python-indent-guess-indent-offset nil)
    (setq python-indent-offset 4)
    (setq python-fill-docstring-style 'django)

    (push "jupyter" python-shell-completion-native-disabled-interpreters)

    (define-key python-mode-map [remap python-indent-dedent-line-backspace]  #'config-python-backspace)
    (define-key python-mode-map [remap python-shell-switch-to-shell] #'config-python-repl)
    (define-key inferior-python-mode-map (kbd "C-c C-z") #'config-python-repl-switch-to-source)

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Python*" eos)
                   (display-buffer-reuse-window
                    display-buffer-at-bottom)
                   (reusable-frames . visible)
                   (slot            . 0)
                   (window-height   . 0.2)))))

(use-package anaconda-mode
  :straight t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))

  :general
  (:states '(insert normal) :keymaps 'anaconda-mode-map
   "M-." #'anaconda-mode-find-definitions
   "M-," #'pop-tag-mark)
  (:states 'normal :keymaps 'anaconda-mode-map "K" #'anaconda-mode-show-doc)
  (:states 'motion :keymaps 'anaconda-mode-view-mode-map "q" #'quit-window)

  :preface
  (progn
    (autoload 'xref-push-marker-stack "xref")
    (defun config-python--push-mark (&rest _)
      (xref-push-marker-stack)))

  :config
  (progn
    (let ((dir (f-join paths-cache-directory "anaconda-mode")))
      (f-mkdir dir)
      (setq anaconda-mode-installation-directory dir))

    ;; Push a marker when navigating with ananconda so that pop marker commands
    ;; work.
    (advice-add 'anaconda-mode-find-assignments :before #'config-python--push-mark)
    (advice-add 'anaconda-mode-find-definitions :before #'config-python--push-mark)))

(use-package company-anaconda
  :straight t
  :defer t
  :hook (anaconda-mode . config-python--enable-company-anaconda)
  :preface
  (defun config-python--enable-company-anaconda ()
    (with-no-warnings
      (add-to-list 'company-backends 'company-anaconda))))

(use-package pytest
  :straight t
  :defer t
  :after python
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
  :hook (python-mode . config-python-pyvenv-activate-if-found)
  :preface
  (progn
    (autoload 'projectile-project-p "projectile")
    (autoload 'f-join "f")

    (defvar config-python--venv-names '(".env" "env" ".venv" "venv" ".virtualenv"))

    (defun config-python--directory-first-ancestor (dir pred)
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

    (defun config-python--find-venv-in-directory (dir)
      (-when-let ((dir) (--keep (let ((dir (f-join dir it)))
                                  (when (f-directory? dir)
                                    dir))
                                config-python--venv-names))
        (file-truename dir)))

    (defun config-python-pyvenv-dir ()
      (config-python--directory-first-ancestor default-directory
                                  #'config-python--find-venv-in-directory))

    (defun config-python-pyvenv-activate-if-found ()
      (-when-let (env (config-python-pyvenv-dir))
        (pyvenv-activate env)
        (message "Using pyvenv at %s" (f-abbrev env))))

    (defun config-python-pyvenv-init (env)
      (interactive
       (list (or (config-python-pyvenv-dir)
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
           (message "%sFAILED" (aref (cdr reporter) 3))))))))

;; pip install isort

(use-package py-isort
  :straight t
  :hook (python-mode . python-sort-imports-mode)
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
        (remove-hook 'before-save-hook 'python-sort-imports-maybe t)))))

;; pip install yapf

(use-package py-yapf
  :straight t
  :defer t
  :after python
  :hook (python-mode . python-auto-format-mode)
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
        (remove-hook 'before-save-hook 'python-auto-format-maybe t)))))

(provide 'config-python)

;;; config-python.el ends here
