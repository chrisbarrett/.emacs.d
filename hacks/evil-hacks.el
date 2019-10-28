;;; evil-hacks.el --- Hacks for evil-mode.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'el-patch)
(require 'straight)

(use-package link-hint
  :straight t
  :commands (link-hint-open-link-at-point)
  :init
  (autoload 'link-hint--get-link-at-point "link-hint"))

(cl-eval-when (compile)
  (require 'evil)
  (require 'link-hint nil t))

(el-patch-feature evil)

;; Teach evil-ret how to navigate to links.

(with-eval-after-load 'evil
  (el-patch-defun evil-ret-gen (count indent?)
    (let* ((field  (get-char-property (point) 'field))
           (button (get-char-property (point) 'button))
           (doc    (get-char-property (point) 'widget-doc))
           (widget (or field button doc))
           (el-patch-add (link (link-hint--get-link-at-point))))
      (cond
       (el-patch-add
         (link
          (link-hint-open-link-at-point)))

       ((and widget
             (fboundp 'widget-type)
             (fboundp 'widget-button-press)
             (or (and (symbolp widget)
                      (get widget 'widget-type))
                 (and (consp widget)
                      (get (widget-type widget) 'widget-type))))
        (when (evil-operator-state-p)
          (setq evil-inhibit-operator t))
        (when (fboundp 'widget-button-press)
          (widget-button-press (point))))
       ((and (fboundp 'button-at)
             (fboundp 'push-button)
             (button-at (point)))
        (when (evil-operator-state-p)
          (setq evil-inhibit-operator t))
        (push-button))
       ((or (evil-emacs-state-p)
            (and (evil-insert-state-p)
                 (not buffer-read-only)))
        (if (not indent?)
            (newline count)
          (delete-horizontal-space t)
          (newline count)
          (indent-according-to-mode)))
       (t
        (evil-next-line-first-non-blank count))))))

(provide 'evil-hacks)

;;; evil-hacks.el ends here
