;;; cb-general-editing.el --- Configure general editing builtins  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package subr
  :config
  ;; Clean up whitespace when inserting yanked text
  (define-advice insert-for-yank (:after (&rest _))
    (whitespace-cleanup)
    (delete-trailing-whitespace)))

(use-package simple
  :config
  ;; Set reasonable default indentation settings
  (setq-default fill-column 80)
  (setq-default indent-tabs-mode nil))

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package newcomment
  :preface
  (autoload 'thing-at-point-looking-at "thingatpt")
  (define-advice comment-indent-new-line (:after (&rest _) insert space)
    "Insert a leading space after comment start for new comment lines."
    (when (and comment-start
               (thing-at-point-looking-at (regexp-quote comment-start)))
      (unless (or (thing-at-point-looking-at (rx (+ space))))
        (just-one-space)))))

(use-package ffap
  :custom
  ;; Don't try to ping things that look like domain names.
  (ffap-machine-p-known 'reject))

(use-package files
  :custom
  ;; Don't confirm before killing subprocesses on exit.
  (confirm-kill-processes nil)
  :preface
  (define-advice save-buffers-kill-emacs (:around (fn &rest args) suppress-prompt)
    (cl-labels ((process-list () nil))
      (apply fn args)))

  ;; Convert ANSI color codes to text properties in shell output
  ;; (autoload 'ansi-color-apply-on-region "ansi-color")
  ;; (define-advice display-message-or-buffer (:before (buf &rest _) render-ansi)
  ;;   (and (bufferp buf)
  ;;        (string= (buffer-name buf) "*Shell Command Output*")
  ;;        (with-current-buffer buf
  ;;          (ansi-color-apply-on-region (point-min) (point-max)))))

  :custom
  ;; Share the Emacs kill ring with the host OS clipboard.
  (select-enable-clipboard t)
  (save-interprogram-paste-before-kill t)

  ;; Prevent duplicated entries in the kill ring
  (kill-do-not-save-duplicates t)

  ;; Do not truncate the results of `eval-expression'
  (eval-expression-print-length nil)
  (eval-expression-print-level nil))

(use-package pixel-scroll
  :demand t
  :config (pixel-scroll-mode +1))

(use-package saveplace
  :demand t
  :config (save-place-mode +1))

(use-package savehist
  :demand t
  :config (savehist-mode +1)
  :custom
  (savehist-additional-variables '(kill-ring
                                   compile-command
                                   search-ring
                                   regexp-search-ring)))

(use-package autorevert
  :delight (auto-revert-mode " auto-revert")
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil))

;; Turns URLs and mailto links into clickable buttons
(use-package goto-addr
  :hook (prog-mode . goto-address-prog-mode))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :preface
  (use-package advice-utils
    :autoload (advice-ignore-errors))
  :config
  (advice-add 'hs-hide-all :around #'advice-ignore-errors)
  (advice-add 'hs-hide-block :around 'advice-ignore-errors)
  (advice-add 'hs-minor-mode :around #'advice-ignore-errors)
  (advice-add 'hs-show-all :around #'advice-ignore-errors)
  (advice-add 'hs-show-block :around #'advice-ignore-errors)
  (advice-add 'hs-toggle-hiding :around #'advice-ignore-errors))

(use-package compile
  :custom
  (compilation-environment '("TERM=screen-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package ansi-color
  :autoload (ansi-color-apply-on-region)
  :preface
  (defun cb-colourise-compilation-output ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (save-excursion
                                    (goto-char compilation-filter-start)
                                    (line-beginning-position))
                                  (point))))
  :hook (compilation-filter . cb-colourise-compilation-output))

(use-package ediff
  :custom
  ;; Configure how `ediff' should display windows when started.

  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally)

  ;; Teach `ediff' how to copy contents from both buffers in a three-way merge.

  :functions
  (ediff-setup-windows-plain ediff-copy-diff ediff-get-region-contents)

  :preface
  (defun cb-ediff-copy-both-to-C ()
    "Copy both ediff buffers in a 3-way merge to the target buffer."
    (interactive)
    (let ((str
           (concat
            (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
            (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
      (ediff-copy-diff ediff-current-difference nil 'C nil str)))

  :general
  (:keymaps 'ediff-mode-map "B" #'cb-ediff-copy-both-to-C)

  ;; Reveal the context around the selected hunk when diffing org buffers.

  :preface
  (defun cb--org-reveal-around-ediff-hunk (&rest _)
    (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (when (derived-mode-p 'org-mode)
            (org-reveal t))))))
  :config
  (advice-add 'ediff-next-difference :after #'cb--org-reveal-around-ediff-hunk)
  (advice-add 'ediff-previous-difference :after #'cb--org-reveal-around-ediff-hunk))

(provide 'cb-general-editing)

;;; cb-general-editing.el ends here
