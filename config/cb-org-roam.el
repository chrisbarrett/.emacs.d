;;; cb-org-roam.el --- org-roam configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'cb-macs)
(require 'cb-parameters)
(require 'thingatpt)
(require 's)

(cl-eval-when (compile)
  (require 'org-roam))

(defface org-roam-custom-node-topic nil
  "Face for node topics."
  :group 'cb-org-roam)

(use-package org-roam
  :after org
  :demand t
  :custom
  (org-roam-verbose nil)
  (org-roam-extract-new-file-path "notes/%<%Y-%m-%d--%H-%M-%S>.org")
  (org-roam-mode-sections '((org-roam-backlinks-section :unique t)
                            (org-roam-reflinks-section)))

  ;; Keep the DB in sync.
  :config
  (org-roam-db-autosync-enable)


  ;; Allow ID lookups to roam nodes from non-roam org files.

  :preface
  (defun cb-org-roam-ensure-ids-navigable ()
    (add-hook 'after-save-hook
              (lambda ()
                (when-let* ((id (org-entry-get-with-inheritance "ID"))
                            (file (buffer-file-name)))
                  (org-id-add-location id file)))
              nil t))
  :config
  (add-hook 'org-mode-hook #'cb-org-roam-ensure-ids-navigable)
  (pcase-dolist (`(,id ,file)
                 (org-roam-db-query "SELECT id, file FROM nodes"))
    (org-id-add-location id file))

  ;; Prevent C-c C-TAB from toggling table column on roam link insertion
  ;;
  ;; Emacs thinks C-c C-TAB and C-c C-i are the same key code. In org tables,
  ;; this means the keybinding will collapse the current column instead of
  ;; inserting a link.
  ;;
  ;; Change this behaviour so that a roam link is inserted if a region is active
  ;; while in a table.

  :general
  (:keymaps 'org-mode-map :states 'visual
   "C-c C-I" (general-predicate-dispatch 'org-roam-node-insert
               (and (org-at-table-p) (not (region-active-p))) 'org-ctrl-c-tab))

  ;; Apply a CREATED timestamp property to new nodes

  :preface
  (defun cb-org-roam-set-created-timestamp ()
    (org-with-wide-buffer
     (unless (org-entry-get (point) "CREATED")
       (org-set-property "CREATED" (format-time-string (org-time-stamp-format t t))))))
  :config
  (add-hook 'org-roam-capture-new-node-hook #'cb-org-roam-set-created-timestamp)

  ;; Fix buffer promotion function
  ;;
  ;; `org-roam-promote-entire-buffer' will leave stranded content between the
  ;; top-level property drawer and the title. Advise it to fix that.

  :preface
  (define-advice org-roam-promote-entire-buffer (:after (&rest _))
    (save-match-data
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (org-at-property-drawer-p)
         (org-forward-element))
       (unless (thing-at-point-looking-at (rx bol "#+title"))
         (let* ((start (point))
                (end (progn (search-forward-regexp (rx bol "#+title"))
                            (match-beginning 0)))
                (str (buffer-substring start end)))
           (delete-region start end)
           (while (org-at-keyword-p)
             (org-forward-element))
           (newline)
           (insert str))))))

  ;; SPIKE: Define a command to switch between a few window sizes in the
  ;; backlinks buffer.

  :preface
  (defconst cb-org-roam-side-window-step-width 85)

  (defun cb-org-roam-toggle-window-width ()
    "Toggle between large or normal side window size."
    (interactive)
    (if-let* ((win (seq-find
                    (lambda (it) (with-current-buffer (window-buffer it)
                              (derived-mode-p 'org-roam-mode)))
                    (window-list))))
        (window-resize win
                       (- (if (<= (window-width win) cb-org-roam-side-window-default-width)
                              cb-org-roam-side-window-step-width
                            cb-org-roam-side-window-default-width)
                          (window-width win))
                       t)
      (user-error "No org-roam windows")))

  :general
  (:keymaps 'org-roam-mode-map
   "C-c TAB" #'cb-org-roam-toggle-window-width
   "t" #'cb-org-roam-toggle-window-width)

  ;; Keyboard shortcuts

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



(use-package org-roam-review
  :commands (org-roam-review)
  :autoload org-roam-review-buffers
  :defines org-roam-review-maturity-values
  :custom
  (org-roam-review-ignored-tags '("dailies" "litnotes"))
  (org-roam-review-tags-ignored-for-review-buffer '("dailies" "litnotes"))
  (org-roam-review-show-instructions-p nil)
  (org-roam-review-title-formatter (lambda (node)
                                     (-let* (((&plist :title :subject) (config-org-roam-parse-node-title node))
                                             (title (propertize title 'font-lock-face 'magit-section-secondary-heading))
                                             (subject (when subject
                                                        (propertize (concat subject ": ") 'font-lock-face
                                                                    'font-lock-doc-face))))
                                       (concat subject title))))
  :general
  (:states '(normal) :keymaps 'org-roam-review-mode-map
   "/" 'org-roam-review-modify-tags
   "TAB" 'magit-section-cycle
   "g r" 'org-roam-review-refresh
   [remap evil-next-line] 'evil-next-visual-line
   [remap evil-previous-line] 'evil-previous-visual-line
   "C-c r r" '(org-roam-review-accept :wk "accept")
   "C-c r f" '(org-roam-review-forgot :wk "forgot")
   "C-c r u" '(org-roam-review-bury :wk "bury")
   "C-c r x" '(org-roam-review-set-excluded :wk "set excluded")
   "C-c r m" '(org-roam-review-set-memorise :wk "set memorise")
   "C-c r b" '(org-roam-review-set-budding :wk "set budding")
   "C-c r s" '(org-roam-review-set-seedling :wk "set seedling")
   "C-c r e" '(org-roam-review-set-evergreen :wk "set evergreen"))
  (:keymaps 'org-mode-map
   "C-c r r" '(org-roam-review-accept :wk "accept")
   "C-c r f" '(org-roam-review-forgot :wk "forgot")
   "C-c r u" '(org-roam-review-bury :wk "bury")
   "C-c r x" '(org-roam-review-set-excluded :wk "set excluded")
   "C-c r m" '(org-roam-review-set-memorise :wk "set memorise")
   "C-c r b" '(org-roam-review-set-budding :wk "set budding")
   "C-c r s" '(org-roam-review-set-seedling :wk "set seedling")
   "C-c r e" '(org-roam-review-set-evergreen :wk "set evergreen"))
  :config
  (add-hook 'org-roam-review-next-node-selected-hook 'org-roam-buffer--redisplay-h 91))

(use-package org-roam-rewrite
  :after (org-roam-review)
  :demand t
  :custom
  (org-roam-rewrite-rename-without-confirmation-p t)
  :config
  (setq org-roam-rewrite-extract-excluded-tags
        (seq-uniq (append
                   org-roam-rewrite-extract-excluded-tags
                   org-roam-review-maturity-values
                   ;; Note types
                   '("notes" "moc" "outline" "litnotes"))))

  ;; Remove deleted nodes from Git automatically.
  :preface
  (autoload 'magit-call-git "magit-process")
  (autoload 'magit-convert-filename-for-git "magit-git")

  (defun cb-org-roam-handle-deleted-node (args)
    (-let* (((&plist :file :level) args)
            (default-directory (file-name-directory file)))
      (when (zerop level)
        (magit-call-git "rm" (magit-convert-filename-for-git file)))))
  :config
  (add-hook 'org-roam-rewrite-node-removed-functions #'cb-org-roam-handle-deleted-node)

  ;; Ignore possibly-missing topic prefix when checking which backlinks were
  ;; customised

  :preface
  (defun cb-org-roam-normalise-node-title (node-or-title)
    (-let [(&plist :title) (config-org-roam-parse-node-title node-or-title)]
      (replace-regexp-in-string (rx (+ (any space "\n"))) "" (downcase title))))
  :custom
  (org-roam-rewrite-backlink-transformer
   (-lambda ((&plist :prev-node :new-id :new-desc :prev-desc))
     (let* ((norm-titles (cons (cb-org-roam-normalise-node-title (org-roam-node-title prev-node))
                               (seq-map #'cb-org-roam-normalise-node-title
                                        (org-roam-node-aliases prev-node))))
            (desc-customised-p
             (not (seq-contains-p norm-titles (cb-org-roam-normalise-node-title prev-desc)))))

       (list :id new-id :desc (if desc-customised-p prev-desc new-desc))))))

(use-package org-roam-default-headings
  :commands (org-roam-default-headings-populate)
  :preface
  (defun cb-org-roam-ad-format-buffer (fn &rest args)
    (let* ((result (apply fn args))
           (buf (if (bufferp result) result (current-buffer))))
      (with-current-buffer buf
        (org-roam-default-headings-populate)
        (let ((save-silently t)
              (message-log-max))
          (save-buffer)))
      buf))

  (advice-add 'org-roam-node-visit :around #'cb-org-roam-ad-format-buffer)
  (advice-add 'org-roam-preview-visit :around #'cb-org-roam-ad-format-buffer)
  (advice-add 'org-link-open :around #'cb-org-roam-ad-format-buffer)

  (defun cb-org-roam-format-on-save ()
    (add-hook 'before-save-hook 'org-roam-default-headings-populate nil t))

  :hook
  (org-mode . cb-org-roam-format-on-save))

(use-package org-roam-search
  :commands (org-roam-search)
  :custom
  (org-roam-search-ignored-tags '("dailies"))
  :general
  (:keymaps 'org-roam-mode-map :states '(normal motion)
   "s" 'org-roam-search))

(use-package org-roam-lazy-previews
  :after org-roam
  :demand t)

(use-package org-roam-links
  :commands (org-roam-links)

  ;; Update the links buffer automatically during reviews
  :preface
  (define-advice org-roam-node-open (:after (&rest _) update-links)
    (when (get-buffer-window "*org-roam-links*")
      (save-window-excursion
        (org-roam-links)))))

(use-package org-roam-dblocks
  :commands (org-roam-dblocks-autoupdate-mode)

  ;; Don't enable for template files.
  :preface
  (defun cb-org-roam-dblocks-maybe-autoupdate ()
    (unless (or (string-match-p (rx ".template.org" eos) (buffer-name))
                (derived-mode-p 'snippet-mode))
      (org-roam-dblocks-autoupdate-mode +1)))
  :hook (org-mode . cb-org-roam-dblocks-maybe-autoupdate)

  ;; Refresh dblocks on link navigation.
  :preface
  (define-advice org-roam-id-open (:around (fn &rest args)  refresh-dblocks-on-id-open)
    (let* ((id (car args))
           (node (org-roam-node-from-id id))
           (existing-buf (-some->> node (org-roam-node-file) (find-buffer-visiting))))
      (prog1 (apply fn args)
        (when existing-buf
          (unless (buffer-modified-p existing-buf)
            (with-current-buffer existing-buf
              (org-update-all-dblocks))))))))



;;; Citations

(defconst config-bibfiles (list "~/org/org-roam.bib"))

(use-package org
  :general
  (:keymaps 'org-mode-map "C-c @" 'org-cite-insert)
  :custom
  (org-cite-csl-styles-dir (expand-file-name "~/Documents/Zotero/styles/"))
  (org-cite-global-bibliography config-bibfiles))

(use-package citar
  :general
  (:keymaps 'org-mode-map "C-c b" '(citar-insert-citation :wk "Insert citation"))
  :preface
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-display-transform-functions `((("author" "editor") . ,(-compose 'citar--shorten-names 'org-funcs-clean-bibtex-string))
                                       (("title") . org-funcs-clean-bibtex-string)))
  (citar-notes-paths '("~/org/roam/litnotes"))
  (citar-open-note-function 'org-funcs-go-to-litnote-for-key)
  (citar-bibliography config-bibfiles)
  (citar-symbol-separator "  ")
  :config
  (require 'all-the-icons)
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))

  ;; :preface
  ;; (define-advice citar-cache--entries (:override (bibs) fix-missing-method)
  ;;   (apply #'ht-merge (nreverse (mapcar #'citar-cache--bibliography-entries bibs))))

  ;; (define-advice citar-cache--preformatted (:override (bibs) fix-missing-method)
  ;;   "Return hash table containing pre-formatted strings from BIBS."
  ;;   (apply #'ht-merge
  ;;          (nreverse (mapcar #'citar-cache--bibliography-preformatted bibs))))

  )

(use-package citar-org-roam
  :after (:all org-roam citar)
  :demand t
  :config
  (setf (plist-get citar-org-roam-notes-config :create) #'org-funcs-go-to-litnote-for-key)
  (citar-org-roam-mode))

(use-package org-collapse-citations
  :hook (org-mode . cursor-sensor-mode)
  :after (:all org citar-org)
  :demand t
  :config
  (add-to-list 'citar-org-activation-functions #'org-collapse-citations-activation-function t))



(use-package timekeep
  :commands (timekeep-start
             timekeep-stop
             timekeep-mode
             timekeep-visit-node)
  :after org
  :demand t
  :general ("<f12>" (general-predicate-dispatch 'timekeep-start
                      (and (fboundp 'org-clocking-p) (org-clocking-p)) 'timekeep-stop))
  :preface
  (defun config-org--update-node-filters ()
    (when (require 'org-roam-review nil t)
      (org-roam-review-modify-tags (org-tags-filter-parse (if (and timekeep-mode (org-clocking-p))
                                                              "-private"
                                                            "")))))

  :config
  (timekeep-mode +1)
  (add-hook 'timekeep-agenda-should-update-hook #'org-funcs-agenda-dwim)
  (add-hook 'timekeep-punched-out-hook #'config-org--update-node-filters)
  (add-hook 'timekeep-punched-in-hook #'config-org--update-node-filters))

(use-package org-roam-slipbox
  :after org-roam
  :demand t
  :config
  (org-roam-slipbox-tag-mode +1)
  (org-roam-slipbox-buffer-identification-mode +1))



;;; Define a parser for my title conventions

;; I sometimes like to scope notes to a particular topic, using a `TOPIC: NAME'
;; convention. This is different from having separate slipboxes, since I use
;; slipboxes as /types/ (evergreens, stubs & nouns vs lit notes).

(defun config-org-roam-parse-node-title (node-or-title)
  (pcase-let* ((original (if (stringp node-or-title) node-or-title (org-roam-node-title node-or-title)))
               (`(,_ ,subject ,title)
                (s-match (rx (* space) (group (+? nonl)) (* space) (any "/:") (+ space) (group (+ nonl)))
                         original)))
    (if subject
        (list :subject subject :title title)
      (list :title original))))

(use-package org-roam
  ;; Fontify the topic prefix specially if present
  :preface
  (define-advice org-fontify-meta-lines-and-blocks-1 (:after (&rest _) fontify-subject)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp (rx bol "#+title:" (+ space) (group (+? nonl) ":") (+ nonl)) nil t)
        (add-text-properties (match-beginning 1) (match-end 1)
                             '(font-lock-fontified t face org-roam-custom-node-topic)))))

  ;; Insert nodes without the topic prefix if present
  :custom
  (org-roam-node-formatter
   (lambda (node)
     (plist-get (config-org-roam-parse-node-title node) :title)))

  ;; Customise completion UI
  :custom
  (org-roam-node-display-template
   (concat (propertize "@${slipbox:9}" 'face 'org-tag)
           " ${icon:3} "
           " ${formatted-title:*} "
           "${tags:*}"))
  :config
  (cl-defmethod org-roam-node-formatted-title ((node org-roam-node))
    (-let [(result &as &plist :title :subject) (config-org-roam-parse-node-title node)]
      (if subject
          (concat (propertize (concat "/" subject) 'face 'org-property-value) " " title)
        title)))
  (cl-defmethod org-roam-node-icon ((node org-roam-node))
    (condition-case nil
        (when-let* ((maturity (car (seq-intersection org-roam-review-maturity-values (org-roam-node-tags node)))))
          (alist-get maturity org-roam-review-maturity-emoji-alist nil nil #'string=))
      (error ""))))



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
