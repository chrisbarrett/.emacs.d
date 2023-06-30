;;; cb-window-management.el --- display-buffer customisations  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

;; Anchor the cursor to the top or bottom of the window during scrolling, rather
;; than paginating through the buffer.
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 101)

;; Always focus on help windows
(setq help-window-select t)

;; Don't show 'press q to close' message
(advice-add 'help-window-display-message :override #'ignore)



(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)

;; Define a full-frame display-buffer action

(defun cb-window-management-fullframe (buffer alist)
  (when-let* ((window (or (display-buffer-reuse-window buffer alist)
                          (display-buffer-same-window buffer alist)
                          (display-buffer-pop-up-window buffer alist)
                          (display-buffer-use-some-window buffer alist))))
    (delete-other-windows window)
    window))

;; Prevent display-buffer from creating new frames

(defun cb-window-management-fallback (buffer &rest _)
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
         cb-window-management-fallback)))

(defun cb-window-management-set (regexp-or-predicate actions)
  (setf (alist-get regexp-or-predicate display-buffer-alist nil nil #'equal) actions))



;;; Customisations

;; Compilation

(cb-window-management-set (lambda (buf _)
                         (with-current-buffer buf
                           (derived-mode-p 'compilation-mode)))
                       '((display-buffer-in-side-window)
                         (side . bottom)
                         (dedicated . t)
                         (window-height . 0.4)))

;; org-babel

(cb-window-management-set (rx bos "*Org-Babel Error Output*" eos)
                       '((display-buffer-in-side-window)
                         (side . bottom)
                         (dedicated . t)
                         (window-height . 0.4)))

;; Lisp debugger

(cb-window-management-set (rx bos "*Backtrace*" eos)
                       '((display-buffer-in-direction)
                         (inhibit-same-window . t)
                         (side . bottom)
                         (window-height . 0.4)))

;; Flymake

(cb-window-management-set (rx bos "*Flymake diagnostics ")
                       '((display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (side . bottom)
                         (window-height . 0.4)))

;; Help

(cb-window-management-set (rx bos (or "*Help*"
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

(cb-window-management-set (rx bos "*ielm*" eos)
                       '((display-buffer-in-direction)
                         (inhibit-same-window . t)
                         (slot . 1)
                         (side . rightmost)
                         (window-height . 0.4)
                         (window-width . 80)))

;; cider

(cb-window-management-set (rx bos "*cider-repl ")
                       '((display-buffer-in-direction)
                         (inhibit-same-window . t)
                         (slot . 1)
                         (side . rightmost)
                         (window-height . 0.4)
                         (window-width . 80)))

;; nix-repl

(cb-window-management-set (rx bos "*Nix-REPL*" eos)
                       '((display-buffer-at-bottom)
                         (inhibit-same-window . t)
                         (window-height   . 0.4)))

;; chatgpt

(cb-window-management-set (rx bos (or "*dall-e*" "*chatgpt*") eos)
                       '((display-buffer-reuse-window
                          (lambda (buffer alist)
                            (when-let* ((win (display-buffer-pop-up-window buffer alist)))
                              (select-window win))))
                         (side . right)
                         (direction . right)))

;; org-roam etc

(defconst config-org-roam-side-window-default-width 55)
(defconst config-org-roam-side-window-breakpoint (+ config-org-roam-side-window-default-width 80))

(cb-window-management-set (rx bos "*org-roam-review*" eos)
                       '(display-buffer-reuse-window
                         cb-window-management-fullframe))

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
  (cb-window-management-set (rx bos "*org-roam" (any "*:")) (make-actions))
  (cb-window-management-set (rx bos "*org-roam-links*" eos) (make-actions))
  (cb-window-management-set (rx bos "*org-roam-search*" eos) (make-actions :slot 2 :window-height 0.7)))

;; pp

(cb-window-management-set (rx bos "*Pp Eval Output*" eos)
                       '((display-buffer-at-bottom)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))

(provide 'cb-window-management)

;;; cb-window-management.el ends here
