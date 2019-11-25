;;; org-hacks.el --- Hacks for org-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-archive)
  (require 'org-capture)
  (require 'org-agenda)
  (require 'org-clock)
  (require 'org-crypt)
  (require 'org-inlinetask))

(defvar org-hacks--bullet (concat "⦿"))

(el-patch-feature org)

(with-eval-after-load 'org
  (el-patch-defun org-font-lock-add-priority-faces (limit)
    "Add the special priority faces."
    (while (re-search-forward "^\\*+ .*?\\(\\[#\\(.\\)\\]\\)" limit t)
      (add-text-properties
       (match-beginning 1) (match-end 1)
       (list 'face (org-get-priority-face (string-to-char (match-string 2)))
             'font-lock-fontified t
             (el-patch-add 'display org-hacks--bullet))))))

(el-patch-feature org-agenda)

(with-eval-after-load 'org-agenda
  (el-patch-defun org-agenda-fontify-priorities ()
    "Make highest priority lines bold, and lowest italic."
    (interactive)
    (mapc (lambda (o) (when (eq (overlay-get o 'org-type) 'org-priority)
                   (delete-overlay o)))
          (overlays-in (point-min) (point-max)))
    (save-excursion
      (let (b e p ov h l)
        (goto-char (point-min))
        (while (re-search-forward "\\[#\\(.\\)\\]" nil t)
          (setq h (or (get-char-property (point) 'org-highest-priority)
                      org-highest-priority)
                l (or (get-char-property (point) 'org-lowest-priority)
                      org-lowest-priority)
                p (string-to-char (match-string 1))
                b (match-beginning 0)
                e (if (eq org-agenda-fontify-priorities 'cookies)
                      (match-end 0)
                    (point-at-eol))
                ov (make-overlay b e))
          (overlay-put
           ov 'face
           (let ((special-face
                  (cond ((org-face-from-face-or-color
                          'priority 'org-priority
                          (cdr (assoc p org-priority-faces))))
                        ((and (listp org-agenda-fontify-priorities)
                              (org-face-from-face-or-color
                               'priority 'org-priority
                               (cdr (assoc p org-agenda-fontify-priorities)))))
                        ((equal p l) 'italic)
                        ((equal p h) 'bold))))
             (if special-face (list special-face 'org-priority) 'org-priority)))
          (el-patch-add (overlay-put ov 'display org-hacks--bullet))
          (overlay-put ov 'org-type 'org-priority))))))

(el-patch-feature org-archive)

(defun org-hacks--remove-prioritie-cookies-in-tree ()
  (ignore-errors
    (org-priority ?\s))
  (org-map-tree (lambda ()
                  (ignore-errors
                    (org-priority ?\s)))))

(with-eval-after-load 'org-archive
  (el-patch-defun org-archive-subtree (&optional find-done)
    "Move the current subtree to the archive.
The archive can be a certain top-level heading in the current
file, or in a different file.  The tree will be moved to that
location, the subtree heading be marked DONE, and the current
time will be added.

When called with a single prefix argument FIND-DONE, find whole
trees without any open TODO items and archive them (after getting
confirmation from the user).  When called with a double prefix
argument, find whole trees with timestamps before today and
archive them (after getting confirmation from the user).  If the
cursor is not at a headline when these commands are called, try
all level 1 trees.  If the cursor is on a headline, only try the
direct children of this heading."
    (interactive "P")
    (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
        (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
                      'region-start-level 'region))
              org-loop-over-headlines-in-active-region)
          (org-map-entries
           `(progn (setq org-map-continue-from (progn (org-back-to-heading) (point)))
                   (org-archive-subtree ,find-done))
           org-loop-over-headlines-in-active-region
           cl (if (org-invisible-p) (org-end-of-subtree nil t))))
      (cond
       ((equal find-done '(4))  (org-archive-all-done))
       ((equal find-done '(16)) (org-archive-all-old))
       (t
        ;; Save all relevant TODO keyword-related variables.
        (let* ((tr-org-todo-keywords-1 org-todo-keywords-1)
               (tr-org-todo-kwd-alist org-todo-kwd-alist)
               (tr-org-done-keywords org-done-keywords)
               (tr-org-todo-regexp org-todo-regexp)
               (tr-org-todo-line-regexp org-todo-line-regexp)
               (tr-org-odd-levels-only org-odd-levels-only)
               (this-buffer (current-buffer))
               (time (format-time-string
                      (substring (cdr org-time-stamp-formats) 1 -1)))
               (file (abbreviate-file-name
                      (or (buffer-file-name (buffer-base-buffer))
                          (error "No file associated to buffer"))))
               (location (org-archive--compute-location
                          (or (org-entry-get nil "ARCHIVE" 'inherit)
                              org-archive-location)))
               (afile (car location))
               (heading (cdr location))
               (infile-p (equal file (abbreviate-file-name (or afile ""))))
               (newfile-p (and (org-string-nw-p afile)
                               (not (file-exists-p afile))))
               (buffer (cond ((not (org-string-nw-p afile)) this-buffer)
                             ((find-buffer-visiting afile))
                             ((find-file-noselect afile))
                             (t (error "Cannot access file \"%s\"" afile))))
               level datetree-date datetree-subheading-p)
          (when (string-match "\\`datetree/" heading)
            ;; Replace with ***, to represent the 3 levels of headings the
            ;; datetree has.
            (setq heading (replace-regexp-in-string "\\`datetree/" "***" heading))
            (setq datetree-subheading-p (> (length heading) 3))
            (setq datetree-date (org-date-to-gregorian
                                 (or (org-entry-get nil "CLOSED" t) time))))
          (if (and (> (length heading) 0)
                   (string-match "^\\*+" heading))
              (setq level (match-end 0))
            (setq heading nil level 0))
          (save-excursion
            (org-back-to-heading t)
            ;; Get context information that will be lost by moving the
            ;; tree.  See `org-archive-save-context-info'.
            (let* ((all-tags (org-get-tags))
                   (local-tags
                    (cl-remove-if (lambda (tag)
                                    (get-text-property 0 'inherited tag))
                                  all-tags))
                   (inherited-tags
                    (cl-remove-if-not (lambda (tag)
                                        (get-text-property 0 'inherited tag))
                                      all-tags))
                   (context
                    `((category . ,(org-get-category nil 'force-refresh))
                      (file . ,file)
                      (itags . ,(mapconcat #'identity inherited-tags " "))
                      (ltags . ,(mapconcat #'identity local-tags " "))
                      (olpath . ,(mapconcat #'identity
                                            (org-get-outline-path)
                                            "/"))
                      (time . ,time)
                      (todo . ,(org-entry-get (point) "TODO")))))
              ;; We first only copy, in case something goes wrong
              ;; we need to protect `this-command', to avoid kill-region sets it,
              ;; which would lead to duplication of subtrees
              (let (this-command) (org-copy-subtree 1 nil t))
              (set-buffer buffer)
              ;; Enforce Org mode for the archive buffer
              (if (not (derived-mode-p 'org-mode))
                  ;; Force the mode for future visits.
                  (let ((org-insert-mode-line-in-empty-file t)
                        (org-inhibit-startup t))
                    (call-interactively 'org-mode)))
              (when (and newfile-p org-archive-file-header-format)
                (goto-char (point-max))
                (insert (format org-archive-file-header-format
                                (buffer-file-name this-buffer))))
              (when datetree-date
                (require 'org-datetree)
                (org-datetree-find-date-create datetree-date)
                (org-narrow-to-subtree))
              ;; Force the TODO keywords of the original buffer
              (let ((org-todo-line-regexp tr-org-todo-line-regexp)
                    (org-todo-keywords-1 tr-org-todo-keywords-1)
                    (org-todo-kwd-alist tr-org-todo-kwd-alist)
                    (org-done-keywords tr-org-done-keywords)
                    (org-todo-regexp tr-org-todo-regexp)
                    (org-todo-line-regexp tr-org-todo-line-regexp)
                    (org-odd-levels-only
                     (if (local-variable-p 'org-odd-levels-only (current-buffer))
                         org-odd-levels-only
                       tr-org-odd-levels-only)))
                (goto-char (point-min))
                (org-show-all '(headings blocks))
                (if (and heading (not (and datetree-date (not datetree-subheading-p))))
                    (progn
                      (if (re-search-forward
                           (concat "^" (regexp-quote heading)
                                   "\\([ \t]+:\\(" org-tag-re ":\\)+\\)?[ \t]*$")
                           nil t)
                          (goto-char (match-end 0))
                        ;; Heading not found, just insert it at the end
                        (goto-char (point-max))
                        (or (bolp) (insert "\n"))
                        ;; datetrees don't need too much spacing
                        (insert (if datetree-date "" "\n") heading "\n")
                        (end-of-line 0))
                      ;; Make the subtree visible
                      (outline-show-subtree)
                      (if org-archive-reversed-order
                          (progn
                            (org-back-to-heading t)
                            (outline-next-heading))
                        (org-end-of-subtree t))
                      (skip-chars-backward " \t\r\n")
                      (and (looking-at "[ \t\r\n]*")
                           ;; datetree archives don't need so much spacing.
                           (replace-match (if datetree-date "\n" "\n\n"))))
                  ;; No specific heading, just go to end of file, or to the
                  ;; beginning, depending on `org-archive-reversed-order'.
                  (if org-archive-reversed-order
                      (progn
                        (goto-char (point-min))
                        (unless (org-at-heading-p) (outline-next-heading)))
                    (goto-char (point-max))
                    ;; Subtree narrowing can let the buffer end on
                    ;; a headline.  `org-paste-subtree' then deletes it.
                    ;; To prevent this, make sure visible part of buffer
                    ;; always terminates on a new line, while limiting
                    ;; number of blank lines in a date tree.
                    (unless (and datetree-date (bolp)) (insert "\n"))))
                ;; Paste
                (org-paste-subtree (org-get-valid-level level (and heading 1)))
                ;; Shall we append inherited tags?
                (and inherited-tags
                     (or (and (eq org-archive-subtree-add-inherited-tags 'infile)
                              infile-p)
                         (eq org-archive-subtree-add-inherited-tags t))
                     (org-set-tags all-tags))
                ;; Mark the entry as done
                (when (and org-archive-mark-done
                           (let ((case-fold-search nil))
                             (looking-at org-todo-line-regexp))
                           (or (not (match-end 2))
                               (not (member (match-string 2) org-done-keywords))))
                  (let (org-log-done org-todo-log-states)
                    (org-todo
                     (car (or (member org-archive-mark-done org-done-keywords)
                              org-done-keywords)))))

                ;; Add the context info.
                (dolist (item org-archive-save-context-info)
                  (let ((value (cdr (assq item context))))
                    (when (org-string-nw-p value)
                      (org-entry-put
                       (point)
                       (concat "ARCHIVE_" (upcase (symbol-name item)))
                       value))))
                (widen)
                ;; Remove priority cookies.
                (el-patch-add
                  (when (org-at-heading-p)
                    (org-hacks--remove-prioritie-cookies-in-tree))))))
          ;; Here we are back in the original buffer.  Everything seems
          ;; to have worked.  So now run hooks, cut the tree and finish
          ;; up.
          (run-hooks 'org-archive-hook)
          (let (this-command) (org-cut-subtree))
          (when (featurep 'org-inlinetask)
            (org-inlinetask-remove-END-maybe))
          (setq org-markers-to-move nil)
          (when org-provide-todo-statistics
            (save-excursion
              ;; Go to parent, even if no children exist.
              (org-up-heading-safe)
              ;; Update cookie of parent.
              (org-update-statistics-cookies nil)))
          (message "Subtree archived %s"
                   (if (eq this-buffer buffer)
                       (concat "under heading: " heading)
                     (concat "in file: " (abbreviate-file-name afile)))))))
      (org-reveal)
      (if (looking-at "^[ \t]*$")
          (outline-next-visible-heading 1)))))

(with-eval-after-load 'org-capture
  (el-patch-defun org-capture-finalize (&optional stay-with-capture)
    "Finalize the capture process.
With prefix argument STAY-WITH-CAPTURE, jump to the location of the
captured item after finalizing."
    (interactive "P")
    (when (org-capture-get :jump-to-captured)
      (setq stay-with-capture t))
    (unless (and org-capture-mode
                 (buffer-base-buffer (current-buffer)))
      (error "This does not seem to be a capture buffer for Org mode"))

    (run-hooks 'org-capture-prepare-finalize-hook)

    ;; Did we start the clock in this capture buffer?
    (when (and org-capture-clock-was-started
               org-clock-marker
               (eq (marker-buffer org-clock-marker) (buffer-base-buffer))
               (>= org-clock-marker (point-min))
               (< org-clock-marker (point-max)))
      ;; Looks like the clock we started is still running.
      (if org-capture-clock-keep
          ;; User may have completed clocked heading from the template.
          ;; Refresh clock mode line.
          (org-clock-update-mode-line t)
        ;; Clock out.  Possibly resume interrupted clock.
        (let (org-log-note-clock-out) (org-clock-out))
        (when (and (org-capture-get :clock-resume 'local)
                   (markerp (org-capture-get :interrupted-clock 'local))
                   (buffer-live-p (marker-buffer
                                   (org-capture-get :interrupted-clock 'local))))
          (let ((clock-in-task (org-capture-get :interrupted-clock 'local)))
            (org-with-point-at clock-in-task (org-clock-in)))
          (message "Interrupted clock has been resumed"))))

    (let ((beg (point-min))
          (end (point-max))
          (abort-note nil))
      ;; Store the size of the capture buffer
      (org-capture-put :captured-entry-size (- (point-max) (point-min)))
      (widen)
      ;; Store the insertion point in the target buffer
      (org-capture-put :insertion-point (point))

      (if org-note-abort
          (let ((m1 (org-capture-get :begin-marker 'local))
                (m2 (org-capture-get :end-marker 'local)))
            (if (and m1 m2 (= m1 beg) (= m2 end))
                (progn
                  (setq
                   ;; PATCH: Fix cleanup action deleting start of next heading.
                   (el-patch-remove m2
                                    (if (cdr (assq 'heading org-blank-before-new-entry))
                                        m2 (1+ m2)))
                   m2 (if (< (point-max) m2) (point-max) m2))
                  (setq abort-note 'clean)
                  (kill-region m1 m2))
              (setq abort-note 'dirty)))

        ;; Postprocessing:  Update Statistics cookies, do the sorting
        (when (derived-mode-p 'org-mode)
          (save-excursion
            (when (ignore-errors (org-back-to-heading))
              (org-update-parent-todo-statistics)
              (org-update-checkbox-count)))
          ;; FIXME Here we should do the sorting
          ;; If we have added a table line, maybe recompute?
          (when (and (eq (org-capture-get :type 'local) 'table-line)
                     (org-at-table-p))
            (if (org-table-get-stored-formulas)
                (org-table-recalculate 'all) ;; FIXME: Should we iterate???
              (org-table-align))))
        ;; Store this place as the last one where we stored something
        ;; Do the marking in the base buffer, so that it makes sense after
        ;; the indirect buffer has been killed.
        (org-capture-store-last-position)

        ;; Run the hook
        (run-hooks 'org-capture-before-finalize-hook))

      (when (org-capture-get :decrypted)
        (save-excursion
          (goto-char (org-capture-get :decrypted))
          (org-encrypt-entry)))

      ;; Kill the indirect buffer
      (save-buffer)
      (let ((return-wconf (org-capture-get :return-to-wconf 'local))
            (new-buffer (org-capture-get :new-buffer 'local))
            (kill-buffer (org-capture-get :kill-buffer 'local))
            (base-buffer (buffer-base-buffer (current-buffer))))

        ;; Kill the indirect buffer
        (kill-buffer (current-buffer))

        ;; Narrow back the target buffer to its previous state
        (with-current-buffer (org-capture-get :buffer)
          (let ((reg (org-capture-get :initial-target-region))
                (pos (org-capture-get :initial-target-position))
                (ipt (org-capture-get :insertion-point))
                (size (org-capture-get :captured-entry-size)))
            (if (not reg)
                (widen)
              (cond ((< ipt (car reg))
                     ;; insertion point is before the narrowed region
                     (narrow-to-region (+ size (car reg)) (+ size (cdr reg))))
                    ((> ipt (cdr reg))
                     ;; insertion point is after the narrowed region
                     (narrow-to-region (car reg) (cdr reg)))
                    (t
                     ;; insertion point is within the narrowed region
                     (narrow-to-region (car reg) (+ size (cdr reg)))))
              ;; now place back the point at its original position
              (if (< ipt (car reg))
                  (goto-char (+ size pos))
                (goto-char (if (< ipt pos) (+ size pos) pos))))))

        ;; Kill the target buffer if that is desired
        (when (and base-buffer new-buffer kill-buffer)
          (with-current-buffer base-buffer (save-buffer))
          (kill-buffer base-buffer))

        ;; Restore the window configuration before capture
        (set-window-configuration return-wconf))

      (run-hooks 'org-capture-after-finalize-hook)
      ;; Special cases
      (cond
       (abort-note
        (cl-case abort-note
          (clean
           (message "Capture process aborted and target buffer cleaned up"))
          (dirty
           (error "Capture process aborted, but target buffer could not be \
cleaned up correctly"))))
       (stay-with-capture
        (org-capture-goto-last-stored)))
      ;; Return if we did store something
      (not abort-note))))

;; The ditaa on Nixpkgs is a self-contained script wrapping the java invocation.
;; Hack the babel execute function so that it calls that instead.

(with-eval-after-load 'ob-ditaa
  (el-patch-defun org-babel-execute:ditaa (body params)
    "Execute a block of Ditaa code with org-babel.
This function is called by `org-babel-execute-src-block'."
    (let* ((out-file (or (cdr (assq :file params))
                         (error
                          "ditaa code block requires :file header argument")))
           (cmdline (cdr (assq :cmdline params)))

           (el-patch-remove (java (cdr (assq :java params))))
           (in-file (org-babel-temp-file "ditaa-"))
           (eps (cdr (assq :eps params)))
           (eps-file (when eps
                       (org-babel-process-file-name (concat in-file ".eps"))))
           (pdf-cmd (when (and (or (string= (file-name-extension out-file) "pdf")
                                   (cdr (assq :pdf params))))
                      (concat
                       "epstopdf"
                       " " eps-file
                       " -o=" (org-babel-process-file-name out-file))))
           (cmd (concat (el-patch-remove org-babel-ditaa-java-cmd
                                         " " java " " org-ditaa-jar-option " "
                                         (shell-quote-argument
                                          (expand-file-name
                                           (if eps org-ditaa-eps-jar-path org-ditaa-jar-path))))
                        (el-patch-add "ditaa")
                        " " cmdline
                        " " (org-babel-process-file-name in-file)
                        " " (if pdf-cmd
                                eps-file
                              (org-babel-process-file-name out-file)))))
      (el-patch-remove (unless (file-exists-p org-ditaa-jar-path)
                         (error "Could not find ditaa.jar at %s" org-ditaa-jar-path)))
      (with-temp-file in-file (insert body))
      (message cmd) (shell-command cmd)
      (when pdf-cmd (message pdf-cmd) (shell-command pdf-cmd))
      nil)) ;; signal that output has already been written to file
  )

;; ...and similarly for PlantUML.

(with-eval-after-load 'ob-plantuml
  (el-patch-defun org-babel-execute:plantuml (body params)
    "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
    (let* ((out-file (or (cdr (assq :file params))
                         (error "PlantUML requires a \":file\" header argument")))
           (cmdline (cdr (assq :cmdline params)))
           (in-file (org-babel-temp-file "plantuml-"))
           (el-patch-remove (java (or (cdr (assq :java params)) "")))
           (full-body (org-babel-plantuml-make-body body params))
           (cmd (el-patch-splice 3 (if (string= "" org-plantuml-jar-path)
                                       (error "`org-plantuml-jar-path' is not set")
                                     (concat (el-patch-remove "java " java " -jar "
                                                              (shell-quote-argument
                                                               (expand-file-name org-plantuml-jar-path)))
                                             (el-patch-add "plantuml ")
                                             (if (string= (file-name-extension out-file) "png")
                                                 " -tpng" "")
                                             (if (string= (file-name-extension out-file) "svg")
                                                 " -tsvg" "")
                                             (if (string= (file-name-extension out-file) "eps")
                                                 " -teps" "")
                                             (if (string= (file-name-extension out-file) "pdf")
                                                 " -tpdf" "")
                                             (if (string= (file-name-extension out-file) "tex")
                                                 " -tlatex" "")
                                             (if (string= (file-name-extension out-file) "vdx")
                                                 " -tvdx" "")
                                             (if (string= (file-name-extension out-file) "xmi")
                                                 " -txmi" "")
                                             (if (string= (file-name-extension out-file) "scxml")
                                                 " -tscxml" "")
                                             (if (string= (file-name-extension out-file) "html")
                                                 " -thtml" "")
                                             (if (string= (file-name-extension out-file) "txt")
                                                 " -ttxt" "")
                                             (if (string= (file-name-extension out-file) "utxt")
                                                 " -utxt" "")
                                             " -p " cmdline " < "
                                             (org-babel-process-file-name in-file)
                                             " > "
                                             (org-babel-process-file-name out-file))))))
      (unless (file-exists-p org-plantuml-jar-path)
        (error "Could not find plantuml.jar at %s" org-plantuml-jar-path))
      (with-temp-file in-file (insert full-body))
      (message "%s" cmd) (org-babel-eval cmd "")
      nil)) ;; signal that output has already been written to file
  )

;; KLUDGE: not sure why, but this became necessary in recent orgs.

(defun org-hacks--fix-org-src-block-edit (fn &rest args)
  (let ((element-type (ignore-errors
                        (org-element-type (org-element-context (org-element-at-point))))))
    (if (equal 'src-block element-type)
        (org-edit-src-code)
      (funcall fn args))))

(with-eval-after-load 'org
  (advice-add 'org-edit-special :around #'org-hacks--fix-org-src-block-edit))

;; HACK: Stop org from setting repeating tasks to DONE with org-edna.

(with-eval-after-load 'org
  (el-patch-defun org-todo (&optional arg)
    "Change the TODO state of an item.

The state of an item is given by a keyword at the start of the heading,
like
     *** TODO Write paper
     *** DONE Call mom

The different keywords are specified in the variable `org-todo-keywords'.
By default the available states are \"TODO\" and \"DONE\".  So, for this
example: when the item starts with TODO, it is changed to DONE.
When it starts with DONE, the DONE is removed.  And when neither TODO nor
DONE are present, add TODO at the beginning of the heading.
You can set up single-charcter keys to fast-select the new state.  See the
`org-todo-keywords' and `org-use-fast-todo-selection' for details.

With `\\[universal-argument]' prefix ARG, force logging the state change \
and take a
logging note.
With a `\\[universal-argument] \\[universal-argument]' prefix, switch to the \
next set of TODO \
keywords (nextset).
Another way to achieve this is `S-C-<right>'.
With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix, circumvent any state blocking.
With numeric prefix arg, switch to the Nth state.

With a numeric prefix arg of 0, inhibit note taking for the change.
With a numeric prefix arg of -1, cancel repeater to allow marking as DONE.

When called through ELisp, arg is also interpreted in the following way:
`none'        -> empty state
\"\"            -> switch to empty state
`done'        -> switch to DONE
`nextset'     -> switch to the next set of keywords
`previousset' -> switch to the previous set of keywords
\"WAITING\"     -> switch to the specified keyword, but only if it
                 really is a member of `org-todo-keywords'."
    (interactive "P")
    (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
        (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
                      'region-start-level 'region))
              org-loop-over-headlines-in-active-region)
          (org-map-entries
           `(org-todo ,arg)
           org-loop-over-headlines-in-active-region
           cl (when (org-invisible-p) (org-end-of-subtree nil t))))
      (when (equal arg '(16)) (setq arg 'nextset))
      (when (equal arg -1) (org-cancel-repeater) (setq arg nil))
      (let ((org-blocker-hook org-blocker-hook)
            commentp
            case-fold-search)
        (when (equal arg '(64))
          (setq arg nil org-blocker-hook nil))
        (when (and org-blocker-hook
                   (or org-inhibit-blocking
                       (org-entry-get nil "NOBLOCKING")))
          (setq org-blocker-hook nil))
        (save-excursion
          (catch 'exit
            (org-back-to-heading t)
            (when (org-in-commented-heading-p t)
              (org-toggle-comment)
              (setq commentp t))
            (when (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
            (or (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
                (looking-at "\\(?: *\\|[ \t]*$\\)"))
            (let* ((match-data (match-data))
                   (startpos (copy-marker (line-beginning-position)))
                   (force-log (and  (equal arg '(4)) (prog1 t (setq arg nil))))
                   (logging (save-match-data (org-entry-get nil "LOGGING" t t)))
                   (org-log-done org-log-done)
                   (org-log-repeat org-log-repeat)
                   (org-todo-log-states org-todo-log-states)
                   (org-inhibit-logging
                    (if (equal arg 0)
                        (progn (setq arg nil) 'note) org-inhibit-logging))
                   (this (match-string 1))
                   (hl-pos (match-beginning 0))
                   (head (org-get-todo-sequence-head this))
                   (ass (assoc head org-todo-kwd-alist))
                   (interpret (nth 1 ass))
                   (done-word (nth 3 ass))
                   (final-done-word (nth 4 ass))
                   (org-last-state (or this ""))
                   (completion-ignore-case t)
                   (member (member this org-todo-keywords-1))
                   (tail (cdr member))
                   (org-state (cond
                               ((eq arg 'right)
                                ;; Next state
                                (if this
                                    (if tail (car tail) nil)
                                  (car org-todo-keywords-1)))
                               ((eq arg 'left)
                                ;; Previous state
                                (unless (equal member org-todo-keywords-1)
                                  (if this
                                      (nth (- (length org-todo-keywords-1)
                                              (length tail) 2)
                                           org-todo-keywords-1)
                                    (org-last org-todo-keywords-1))))
                               (arg
                                ;; User or caller requests a specific state.
                                (cond
                                 ((equal arg "") nil)
                                 ((eq arg 'none) nil)
                                 ((eq arg 'done) (or done-word (car org-done-keywords)))
                                 ((eq arg 'nextset)
                                  (or (car (cdr (member head org-todo-heads)))
                                      (car org-todo-heads)))
                                 ((eq arg 'previousset)
                                  (let ((org-todo-heads (reverse org-todo-heads)))
                                    (or (car (cdr (member head org-todo-heads)))
                                        (car org-todo-heads))))
                                 ((car (member arg org-todo-keywords-1)))
                                 ((stringp arg)
                                  (user-error "State `%s' not valid in this file" arg))
                                 ((nth (1- (prefix-numeric-value arg))
                                       org-todo-keywords-1))))
                               ((and org-todo-key-trigger org-use-fast-todo-selection)
                                ;; Use fast selection.
                                (org-fast-todo-selection this))
                               ((null member) (or head (car org-todo-keywords-1)))
                               ((equal this final-done-word) nil) ;-> make empty
                               ((null tail) nil) ;-> first entry
                               ((memq interpret '(type priority))
                                (if (eq this-command last-command)
                                    (car tail)
                                  (if (> (length tail) 0)
                                      (or done-word (car org-done-keywords))
                                    nil)))
                               (t
                                (car tail))))
                   (org-state (or
                               (run-hook-with-args-until-success
                                'org-todo-get-default-hook org-state org-last-state)
                               org-state))
                   (next (el-patch-swap (if (org-string-nw-p org-state) (concat " " org-state " ") " ")
                                        (cond
                                         ((org-entry-get (point) "TRIGGER" nil t)
                                          " TODO ")
                                         ((org-string-nw-p org-state)
                                          (concat " " org-state " "))
                                         (t
                                          " "))))
                   (change-plist (list :type 'todo-state-change :from this :to org-state
                                       :position startpos))
                   dolog now-done-p)
              (when org-blocker-hook
                (let (org-blocked-by-checkboxes block-reason)
                  (setq org-last-todo-state-is-todo
                        (not (member this org-done-keywords)))
                  (unless (save-excursion
                            (save-match-data
                              (org-with-wide-buffer
                               (run-hook-with-args-until-failure
                                'org-blocker-hook change-plist))))
                    (setq block-reason (if org-blocked-by-checkboxes
                                           "contained checkboxes"
                                         (format "\"%s\"" org-block-entry-blocking)))
                    (if (called-interactively-p 'interactive)
                        (user-error "TODO state change from %s to %s blocked (by %s)"
                                    this org-state block-reason)
                      ;; Fail silently.
                      (message "TODO state change from %s to %s blocked (by %s)"
                               this org-state block-reason)
                      (throw 'exit nil)))))
              (store-match-data match-data)
              (replace-match next t t)
              (cond ((equal this org-state)
                     (message "TODO state was already %s" (org-trim next)))
                    ((not (pos-visible-in-window-p hl-pos))
                     (message "TODO state changed to %s" (org-trim next))))
              (unless head
                (setq head (org-get-todo-sequence-head org-state)
                      ass (assoc head org-todo-kwd-alist)
                      interpret (nth 1 ass)
                      done-word (nth 3 ass)
                      final-done-word (nth 4 ass)))
              (when (memq arg '(nextset previousset))
                (message "Keyword-Set %d/%d: %s"
                         (- (length org-todo-sets) -1
                            (length (memq (assoc org-state org-todo-sets) org-todo-sets)))
                         (length org-todo-sets)
                         (mapconcat 'identity (assoc org-state org-todo-sets) " ")))
              (setq org-last-todo-state-is-todo
                    (not (member org-state org-done-keywords)))
              (setq now-done-p (and (member org-state org-done-keywords)
                                    (not (member this org-done-keywords))))
              (and logging (org-local-logging logging))
              (when (or (and (or org-todo-log-states org-log-done)
                             (not (eq org-inhibit-logging t))
                             (not (memq arg '(nextset previousset))))
                        force-log)
                ;; We need to look at recording a time and note.
                (setq dolog (or (if force-log 'note)
                                (nth 1 (assoc org-state org-todo-log-states))
                                (nth 2 (assoc this org-todo-log-states))))
                (when (and (eq dolog 'note) (eq org-inhibit-logging 'note))
                  (setq dolog 'time))
                (when (or (and (not org-state) (not org-closed-keep-when-no-todo))
                          (and org-state
                               (member org-state org-not-done-keywords)
                               (not (member this org-not-done-keywords))))
                  ;; This is now a todo state and was not one before
                  ;; If there was a CLOSED time stamp, get rid of it.
                  (org-add-planning-info nil nil 'closed))
                (when (and now-done-p org-log-done)
                  ;; It is now done, and it was not done before.
                  (org-add-planning-info 'closed (org-current-effective-time))
                  (when (and (not dolog) (eq 'note org-log-done))
                    (org-add-log-setup 'done org-state this 'note)))
                (when (and org-state dolog)
                  ;; This is a non-nil state, and we need to log it.
                  (org-add-log-setup 'state org-state this dolog)))
              ;; Fixup tag positioning.
              (org-todo-trigger-tag-changes org-state)
              (when org-auto-align-tags (org-align-tags))
              (when org-provide-todo-statistics
                (org-update-parent-todo-statistics))
              (when (bound-and-true-p org-clock-out-when-done)
                (org-clock-out-if-current))
              (run-hooks 'org-after-todo-state-change-hook)
              (when (and arg (not (member org-state org-done-keywords)))
                (setq head (org-get-todo-sequence-head org-state)))
              (put-text-property (point-at-bol) (point-at-eol) 'org-todo-head head)
              ;; Do we need to trigger a repeat?
              (when now-done-p
                (when (boundp 'org-agenda-headline-snapshot-before-repeat)
                  ;; This is for the agenda, take a snapshot of the headline.
                  (save-match-data
                    (setq org-agenda-headline-snapshot-before-repeat
                          (org-get-heading))))
                (org-auto-repeat-maybe org-state))
              ;; Fixup cursor location if close to the keyword.
              (when (and (outline-on-heading-p)
                         (not (bolp))
                         (save-excursion (beginning-of-line 1)
                                         (looking-at org-todo-line-regexp))
                         (< (point) (+ 2 (or (match-end 2) (match-end 1)))))
                (goto-char (or (match-end 2) (match-end 1)))
                (and (looking-at " ")
                     (not (looking-at " *:"))
                     (just-one-space)))
              (when org-trigger-hook
                (save-excursion
                  (run-hook-with-args 'org-trigger-hook change-plist)))
              (when commentp (org-toggle-comment)))))))))

(provide 'org-hacks)

;;; org-hacks.el ends here
