;;; cb-display-buffer.el --- display-buffer customisations  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)

;; Define a full-frame display-buffer action

(defun cb-display-buffer-fullframe (buffer alist)
  (when-let* ((window (or (display-buffer-reuse-window buffer alist)
                          (display-buffer-same-window buffer alist)
                          (display-buffer-pop-up-window buffer alist)
                          (display-buffer-use-some-window buffer alist))))
    (delete-other-windows window)
    window))

;; Prevent display-buffer from creating new frames

(defun cb-display-buffer-fallback (buffer &rest _)
  (when-let* ((win (split-window-sensibly)))
    (with-selected-window win
      (switch-to-buffer buffer)
      (help-window-setup (selected-window))))
  t)

(setq display-buffer-fallback-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer-pop-up-window
         cb-display-buffer-fallback)))

(defun cb-display-buffer-set (regexp-or-predicate actions)
  (setf (alist-get regexp-or-predicate display-buffer-alist nil nil #'equal) actions))



;;; Customisations

;; Compilation

(cb-display-buffer-set (lambda (buf _)
                         (with-current-buffer buf
                           (derived-mode-p 'compilation-mode)))
                       '((display-buffer-in-side-window)
                         (side . bottom)
                         (dedicated . t)
                         (window-height . 0.4)))

;; org-babel

(cb-display-buffer-set (rx bos "*Org-Babel Error Output*" eos)
                       '((display-buffer-in-side-window)
                         (side . bottom)
                         (dedicated . t)
                         (window-height . 0.4)))

;; Lisp debugger

(cb-display-buffer-set (rx bos "*Backtrace*" eos)
                       '((display-buffer-in-direction)
                         (inhibit-same-window . t)
                         (side . bottom)
                         (window-height . 0.4)))

;; Flymake

(cb-display-buffer-set (rx bos "*Flymake diagnostics ")
                       '((display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (side . bottom)
                         (window-height . 0.4)))

;; Help

(cb-display-buffer-set (rx bos (or "*Help*"
                                   "*helpful "
                                   "*eldoc*"
                                   "*xref*"))
                       '((display-buffer-reuse-mode-window
                          display-buffer-pop-up-window)
                         (dedicated . t)
                         (modes . (Man-mode helpful-mode help-mode Info-mode))
                         (side . rightmost)
                         (window-width . 80)))

;; ielm

(cb-display-buffer-set (rx bos "*ielm*" eos)
                       '((display-buffer-in-direction)
                         (inhibit-same-window . t)
                         (slot . 1)
                         (side . rightmost)
                         (window-height . 0.4)
                         (window-width . 80)))

;; cider

(cb-display-buffer-set (rx bos "*cider-repl ")
                       '((display-buffer-in-direction)
                         (inhibit-same-window . t)
                         (slot . 1)
                         (side . rightmost)
                         (window-height . 0.4)
                         (window-width . 80)))

;; nix-repl

(cb-display-buffer-set (rx bos "*Nix-REPL*" eos)
                       '((display-buffer-at-bottom)
                         (inhibit-same-window . t)
                         (window-height   . 0.4)))

;; chatgpt

(cb-display-buffer-set (rx bos (or "*dall-e*" "*chatgpt*") eos)
                       '((display-buffer-reuse-window
                          (lambda (buffer alist)
                            (when-let* ((win (display-buffer-pop-up-window buffer alist)))
                              (select-window win))))
                         (side . right)
                         (direction . right)))

;; org-roam etc

(defconst config-org-roam-side-window-default-width 55)
(defconst config-org-roam-side-window-breakpoint (+ config-org-roam-side-window-default-width 80))

(cb-display-buffer-set (rx bos "*org-roam-review*" eos)
                       '(display-buffer-reuse-window
                         cb-display-buffer-fullframe))

(cl-labels ((make-actions (&key window-height (slot 1) (side 'left))
                          `(((lambda (buf &rest args)
                               (funcall (if (< (frame-width) config-org-roam-side-window-breakpoint)
                                            'display-buffer-fullframe
                                          'display-buffer-in-side-window)
                                        buf
                                        (append args '((slot . ,slot)
                                                       (side . ,side)
                                                       (window-height . ,window-height)
                                                       (window-width . ,config-org-roam-side-window-default-width)))))))))
  (cb-display-buffer-set (rx bos "*org-roam" (any "*:")) (make-actions))
  (cb-display-buffer-set (rx bos "*org-roam-links*" eos) (make-actions))
  (cb-display-buffer-set (rx bos "*org-roam-search*" eos) (make-actions :slot 2 :window-height 0.7)))

;; pp

(cb-display-buffer-set (rx bos "*Pp Eval Output*" eos)
                       '((display-buffer-at-bottom)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))

(provide 'cb-display-buffer)

;;; cb-display-buffer.el ends here
