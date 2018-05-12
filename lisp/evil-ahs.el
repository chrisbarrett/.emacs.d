;;; evil-ahs.el --- Supporting commands for ahs.  -*- lexical-binding: t; -*-

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

(defconst evil-ahs--symbol-highlight-transient-state-doc
  "
 %s  [_n_] next  [_N_/_p_] previous        [_r_] change range   [_R_] reset       [_e_] iedit
 %s  [_d_/_D_] next/previous definition")

(defvar-local evil-ahs--last-ahs-highlight-p nil
  "Info on the last searched highlighted symbol.")

(defvar-local evil-ahs--ahs-searching-forward t)


(defun evil-ahs--symbol-highlight-ts-doc ()
  (evil-transient-state-make-doc
   'symbol-highlight
   (format evil-ahs--symbol-highlight-transient-state-doc
           (evil-ahs--symbol-highlight-doc)
           (make-string (length (evil-ahs--symbol-highlight-doc)) 32))))

(defun evil-ahs--ahs-to-iedit ()
  (interactive)
  (cond
   ((fboundp 'evil-iedit-state/iedit-mode)
    (evil-iedit-state/iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   (t
    (ahs-edit-mode t))))

(defun evil-ahs--symbol-highlight-doc ()
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

(defun evil-ahs--integrate-evil-search (forward)
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

(defun evil-ahs--ensure-ahs-enabled-locally ()
  "Ensures ahs is enabled for the local buffer."
  (unless (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)))

(defun evil-ahs--highlight-now-wrapper ()
  "Safe wrapper for ahs-highlight-now"
  (eval '(progn
           (evil-ahs--ensure-ahs-enabled-locally)
           (ahs-highlight-now)) nil))

(defun evil-ahs/enter-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (setq evil-ahs--ahs-searching-forward t)
  (evil-ahs-quick-ahs-forward))

(defun evil-ahs/enter-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (setq evil-ahs--ahs-searching-forward nil)
  (evil-ahs-quick-ahs-forward))

(defun evil-ahs-quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (evil-ahs--quick-ahs-move t))

(defun evil-ahs-quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (evil-ahs--quick-ahs-move nil))

(defun evil-ahs--quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with `auto-highlight-symbol'."
  (cond
   ((eq forward evil-ahs--ahs-searching-forward)
    (evil-ahs--integrate-evil-search t)
    (evil-ahs--highlight-now-wrapper)
    (evil-set-jump)
    (symbol-highlight-transient-state/body)
    (ahs-forward))
   (t
    (evil-ahs--integrate-evil-search nil)
    (evil-ahs--highlight-now-wrapper)
    (evil-set-jump)
    (symbol-highlight-transient-state/body)
    (ahs-backward))))

(defun evil-ahs--ms-on-exit ()
  ;; Restore user search direction state as ahs has exited in a state
  ;; good for <C-s>, but not for 'n' and 'N'"
  (setq isearch-forward evil-ahs--ahs-searching-forward))


;; Finally, define micro state.

(evil-transient-state-define symbol-highlight
  :title "Symbol Highlight Transient State"
  :dynamic-hint (evil-ahs--symbol-highlight-ts-doc)
  :before-exit (evil-ahs--ms-on-exit)
  :bindings
  ("d" ahs-forward-definition)
  ("D" ahs-backward-definition)
  ("e" evil-ahs--ahs-to-iedit :exit t)
  ("n" evil-ahs-quick-ahs-forward)
  ("N" evil-ahs-quick-ahs-backward)
  ("p" evil-ahs-quick-ahs-backward)
  ("R" ahs-back-to-start)
  ("r" ahs-change-range)
  ("q" nil :exit t))


;; Define commands for running highlight symbol.

(defun evil-ahs/highlight-symbol ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (evil-ahs--highlight-now-wrapper)
  (setq evil-ahs--last-ahs-highlight-p (ahs-highlight-p))
  (symbol-highlight-transient-state/body)
  (evil-ahs--integrate-evil-search nil))

(defun evil-ahs/goto-last-searched-symbol ()
  "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
  (interactive)
  (if evil-ahs--last-ahs-highlight-p
      (progn (goto-char (nth 1 evil-ahs--last-ahs-highlight-p))
             (evil-ahs--highlight-now-wrapper)
             (symbol-highlight-transient-state/body))
    (message "No symbol has been searched for now.")))


;; Advise existing commands to integrate with micro state.

(defun evil-ahs--apply-highlight (f &rest args)
  (evil-ahs--highlight-now-wrapper)
  (prog1 (apply f args)
    (evil-ahs--highlight-now-wrapper)
    (setq evil-ahs--last-ahs-highlight-p (ahs-highlight-p))))

(advice-add 'ahs-forward :around #'evil-ahs--apply-highlight)
(advice-add 'ahs-backward :around #'evil-ahs--apply-highlight)
(advice-add 'ahs-forward-definition :around #'evil-ahs--apply-highlight)
(advice-add 'ahs-backward-definition :around #'evil-ahs--apply-highlight)
(advice-add 'ahs-back-to-start :around #'evil-ahs--apply-highlight)
(advice-add 'ahs-change-range :around #'evil-ahs--apply-highlight)

(provide 'evil-ahs)

;;; evil-ahs.el ends here
