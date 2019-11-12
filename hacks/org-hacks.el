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

(provide 'org-hacks)

;;; org-hacks.el ends here
