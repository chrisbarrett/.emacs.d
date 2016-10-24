;;; cb-ahs-micro-state.el --- Supporting commands for ahs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;; Nasty AHS micro state borrowed from Spacemacs.

;;; Code:

(require 'evil-transient-state)
(require 'auto-highlight-symbol)
(require 'iedit)
(require 'evil)

(autoload 'evil-iedit-state/iedit-mode "evil-iedit-state")

(defconst cb-ahs-micro-state--symbol-highlight-transient-state-doc
  "
 %s  [_n_] next  [_N_/_p_] previous        [_r_] change range   [_R_] reset       [_e_] iedit
 %s  [_d_/_D_] next/previous definition")

(defvar-local cb-ahs-micro-state--last-ahs-highlight-p nil
  "Info on the last searched highlighted symbol.")

(defvar-local cb-ahs-micro-state--ahs-searching-forward t)


(defun cb-ahs-micro-state--symbol-highlight-ts-doc ()
  (evil-transient-state-make-doc
   'symbol-highlight
   (format cb-ahs-micro-state--symbol-highlight-transient-state-doc
           (cb-ahs-micro-state--symbol-highlight-doc)
           (make-string (length (cb-ahs-micro-state--symbol-highlight-doc)) 32))))

(defun cb-ahs-micro-state--ahs-to-iedit ()
  (interactive)
  (cond
   ((fboundp 'evil-iedit-state/iedit-mode)
    (evil-iedit-state/iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   (t
    (ahs-edit-mode t))))

(defun cb-ahs-micro-state--symbol-highlight-doc ()
  (let* ((i 0)
         (overlay-count (length ahs-overlay-list))
         (overlay (format "%s" (nth i ahs-overlay-list)))
         (current-overlay (format "%s" ahs-current-overlay))
         (st (ahs-stat))
         (plighter (ahs-current-plugin-prop 'lighter))
         (plugin (format "%s"
                         (cond ((string= plighter "HS")  "Display")
                               ((string= plighter "HSA") "Buffer")
                               ((string= plighter "HSD") "Function"))))
         (face (cond ((string= plighter "HS")  ahs-plugin-defalt-face)
                     ((string= plighter "HSA") ahs-plugin-whole-buffer-face)
                     ((string= plighter "HSD") ahs-plugin-bod-face))))
    (while (not (string= overlay current-overlay))
      (setq i (1+ i))
      (setq overlay (format "%s" (nth i ahs-overlay-list))))
    (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
           (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" "")))
      (concat
       (propertize (format " %s " plugin) 'face face)
       (propertize (format " %s%s " x/y hidden) 'face
                   `(:foreground "#ffffff" :background "#000000"))))))

(defun cb-ahs-micro-state--integrate-evil-search (forward)
  ;; isearch-string is last searched item.  Next time
  ;; "n" is hit we will use this.
  (setq isearch-string
        (concat "\\<" (evil-find-thing forward 'symbol) "\\>")
        isearch-regexp
        (concat "\\<" (evil-find-thing forward 'symbol) "\\>"))
  ;; Next time "n" is hit, go the correct direction.
  (setq isearch-forward forward)
  ;; ahs does a case sensitive search.  We could set
  ;; this, but it would break the user's current
  ;; sensitivity settings.  We could save the setting,
  ;; then next time the user starts a search we could
  ;; restore the setting.
  ;;(setq case-fold-search nil)
  ;; Place the search term into the search rings.
  (isearch-update-ring isearch-string t)
  (evil-push-search-history isearch-string forward)
  ;; Use this search term for empty pattern "%s//replacement/"
  ;; Append case sensitivity
  (setq evil-ex-last-was-search nil
        evil-ex-substitute-pattern `(,(concat isearch-string "\\C")
                                     nil (0 0))))

(defun cb-ahs-micro-state--ensure-ahs-enabled-locally ()
  "Ensures ahs is enabled for the local buffer."
  (unless (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)))

(defun cb-ahs-micro-state--highlight-now-wrapper ()
  "Safe wrapper for ahs-highlight-now"
  (eval '(progn
           (cb-ahs-micro-state--ensure-ahs-enabled-locally)
           (ahs-highlight-now)) nil))

(defun cb-ahs-micro-state/enter-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (setq cb-ahs-micro-state--ahs-searching-forward t)
  (cb-ahs-micro-state-quick-ahs-forward))

(defun cb-ahs-micro-state/enter-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (setq cb-ahs-micro-state--ahs-searching-forward nil)
  (cb-ahs-micro-state-quick-ahs-forward))

(defun cb-ahs-micro-state-quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (cb-ahs-micro-state--quick-ahs-move t))

(defun cb-ahs-micro-state-quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (cb-ahs-micro-state--quick-ahs-move nil))

(defun cb-ahs-micro-state--quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with `auto-highlight-symbol'."
  (cond
   ((eq forward cb-ahs-micro-state--ahs-searching-forward)
    (cb-ahs-micro-state--integrate-evil-search t)
    (cb-ahs-micro-state--highlight-now-wrapper)
    (evil-set-jump)
    (symbol-highlight-transient-state/body)
    (ahs-forward))
   (t
    (cb-ahs-micro-state--integrate-evil-search nil)
    (cb-ahs-micro-state--highlight-now-wrapper)
    (evil-set-jump)
    (symbol-highlight-transient-state/body)
    (ahs-backward))))

(defun cb-ahs-micro-state--ms-on-exit ()
  ;; Restore user search direction state as ahs has exited in a state
  ;; good for <C-s>, but not for 'n' and 'N'"
  (setq isearch-forward cb-ahs-micro-state--ahs-searching-forward))


;; Finally, define micro state.

(evil-transient-state-define symbol-highlight
  :title "Symbol Highlight Transient State"
  :dynamic-hint (cb-ahs-micro-state--symbol-highlight-ts-doc)
  :before-exit (cb-ahs-micro-state--ms-on-exit)
  :bindings
  ("d" ahs-forward-definition)
  ("D" ahs-backward-definition)
  ("e" cb-ahs-micro-state--ahs-to-iedit :exit t)
  ("n" cb-ahs-micro-state-quick-ahs-forward)
  ("N" cb-ahs-micro-state-quick-ahs-backward)
  ("p" cb-ahs-micro-state-quick-ahs-backward)
  ("R" ahs-back-to-start)
  ("r" ahs-change-range)
  ("q" nil :exit t))


;; Define commands for running highlight symbol.

(defun cb-ahs-micro-state/highlight-symbol ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (cb-ahs-micro-state--highlight-now-wrapper)
  (setq cb-ahs-micro-state--last-ahs-highlight-p (ahs-highlight-p))
  (symbol-highlight-transient-state/body)
  (cb-ahs-micro-state--integrate-evil-search nil))

(defun cb-ahs-micro-state/goto-last-searched-symbol ()
  "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
  (interactive)
  (if cb-ahs-micro-state--last-ahs-highlight-p
      (progn (goto-char (nth 1 cb-ahs-micro-state--last-ahs-highlight-p))
             (cb-ahs-micro-state--highlight-now-wrapper)
             (symbol-highlight-transient-state/body))
    (message "No symbol has been searched for now.")))


;; Advise existing commands to integrate with micro state.

(defun cb-ahs-micro-state--apply-highlight (f &rest args)
  (cb-ahs-micro-state--highlight-now-wrapper)
  (prog1 (apply f args)
    (cb-ahs-micro-state--highlight-now-wrapper)
    (setq cb-ahs-micro-state--last-ahs-highlight-p (ahs-highlight-p))))

(advice-add 'ahs-forward :around #'cb-ahs-micro-state--apply-highlight)
(advice-add 'ahs-backward :around #'cb-ahs-micro-state--apply-highlight)
(advice-add 'ahs-forward-definition :around #'cb-ahs-micro-state--apply-highlight)
(advice-add 'ahs-backward-definition :around #'cb-ahs-micro-state--apply-highlight)
(advice-add 'ahs-back-to-start :around #'cb-ahs-micro-state--apply-highlight)
(advice-add 'ahs-change-range :around #'cb-ahs-micro-state--apply-highlight)

(provide 'cb-ahs-micro-state)

;;; cb-ahs-micro-state.el ends here
