;;; cb-org-roam.el --- org-roam configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cb-macs)
(require 'cb-parameters)

(cl-eval-when (compile)
  (require 'org-roam))

(use-package org-roam
  :after org
  :demand t
  :custom
  (org-roam-verbose nil)
  (org-roam-extract-new-file-path "notes/%<%Y-%m-%d--%H-%M-%S>.org")
  (org-roam-mode-sections '((org-roam-backlinks-section :unique t)
                            (org-roam-reflinks-section)))

  :config
  (org-roam-db-autosync-enable)

  :general
  (:states '(insert normal)
   :keymaps 'org-mode-map
   "C-c i" '(org-roam-node-insert :wk "insert org-roam link")
   "C-c TAB"
   (general-predicate-dispatch 'org-roam-node-insert
     (org-at-table-p) 'org-ctrl-c-tab)
   "S-<return>"
   (general-predicate-dispatch 'org-funcs-follow-link-other-window
     (org-at-table-p) 'org-table-copy-down)))



;;; Define a parser for my title conventions

;; I sometimes like to scope notes to a particular topic, using a `TOPIC: NAME'
;; convention. This is different from having separate slipboxes, since I use
;; slipboxes as /types/ (evergreens, stubs & nouns vs lit notes).

(require 's)

(defun config-org-roam-parse-node-title (node-or-title)
  (pcase-let* ((original (if (stringp node-or-title) node-or-title (org-roam-node-title node-or-title)))
               (`(,_ ,subject ,title)
                (s-match (rx (* space) (group (+? nonl)) (* space) (any "/:") (+ space) (group (+ nonl)))
                         original)))
    (if subject
        (list :subject subject :title title)
      (list :title original))))



;;; Use index node as my initial buffer choice.

(defun cb-org-roam-initial-buffers ()
  (let ((inhibit-redisplay t)
        (buf))
    (org-roam-node-visit (org-roam-node-from-id org-roam-index-node-id))
    (setq buf (current-buffer))
    (goto-char (point-min))
    (delete-other-windows)
    (org-roam-review)
    (when-let* ((win (-some->> (org-roam-review-buffers) (car) (get-buffer-window))))
      (select-window win)
      (set-window-dedicated-p win t))
    (display-buffer buf)
    buf))

;;; Leader keys

(defun cb-display-roam-backlinks ()
  "Refresh and show the org-roam buffer."
  (interactive)
  (when-let* ((buf (get-buffer org-roam-buffer)))
    (with-current-buffer buf
      (org-roam-buffer-refresh)))
  (org-roam-buffer-toggle))

(defun cb-display-roam-backlinks-dedicated (node)
  "Display a dedicated backlinks buffer for NODE."
  (interactive (list (if current-prefix-arg
                         (org-roam-node-read nil nil nil 'require-match)
                       (org-roam-node-at-point 'assert))))
  (org-roam-buffer-display-dedicated node))

(mode-leader-set-key :keymaps 'org-roam-mode-map :states '(motion normal)
  "<tab>" '(quit-window :wk "hide")
  "<backtab>" '(org-roam-buffer-display-dedicated :wk "backlinks (dedicated)"))

(mode-leader-set-key :keymaps 'org-mode-map
  "<tab>" '(cb-display-roam-backlinks :wk "backlinks")
  "<backtab>" '(cb-display-roam-backlinks-dedicated :wk "backlinks (dedicated)")
  "<" '(org-roam-dailies-find-previous-note :wk "prev daily")
  ">" '(org-roam-dailies-find-next-note :wk "next daily")
  "J" '(org-roam-dailies-goto-date :wk "goto daily...")

  "E" '(org-roam-rewrite-extract :wk "extract subtree to roam file...")
  "r" '(org-roam-rewrite-rename :wk "rename node...")
  "I" '(org-roam-rewrite-inline :wk "inline node...")
  "D" '(org-roam-rewrite-remove :wk "delete node & redirect links...")

  "i" '(nil :wk "transclusion")
  "i a" '(org-transclusion-add :wk "add")
  "i e" '(org-transclusion-live-sync-start :wk "edit")
  "i o" '(org-transclusion-move-to-source :wk "goto source")
  "i A" '(org-transclusion-add-all :wk "add all")
  "i i" '(org-transclusion-make-from-link :wk "make from link...")
  "i x" '(org-transclusion-remove :wk "remove")
  "i X" '(org-transclusion-remove :wk "remove all")

  "l" '(nil :wk "alias")
  "l a" '(org-roam-alias-add :wk "add alias")
  "l x" '(org-roam-alias-remove :wk "remove alias")

  "k" '(nil :wk "tags")
  "k a" '(org-roam-tag-add :wk "add tag")
  "k x" '(org-roam-tag-remove :wk "remove tag"))

(provide 'cb-org-roam)

;;; cb-org-roam.el ends here
