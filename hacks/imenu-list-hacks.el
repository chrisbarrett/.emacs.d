;;; imenu-list-hacks.el --- Hacky patches to imenu-list.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'el-patch)
(autoload 'imenu--subalist-p "imenu")
(autoload 'imenu-list-get-buffer-create "imenu-list")

(el-patch-feature imenu-list)



;; Tell imenu-list to insert a page-break line after the last nested definition
;; group.

(defun imenu-list-hacks--last-nested-index (index-alist)
  (->> index-alist
       (--map-indexed (cons it-index it))
       (nreverse)
       (--first (imenu--subalist-p (cdr it)))
       (car)))

(defun imenu-list-hacks--insert-rule-if-remaining-items-at-level-0 (depth index last-nested-index)
  (let ((at-top-level (eq depth 0))
        (not-first-element (> index 1))
        (after-last-nested-group (eq (1+ (or last-nested-index 0)) index)))

    (when (and at-top-level not-first-element after-last-nested-group)
      (insert "\n"))))

(with-eval-after-load 'imenu-list
  (with-no-warnings
    (el-patch-defun imenu-list--insert-entries-internal (index-alist depth)
      "Insert all imenu entries in INDEX-ALIST into the current buffer.
DEPTH is the depth of the code block were the entries are written.
Each entry is inserted in its own line.
Each entry is appended to `imenu-list--line-entries' as well."
      (el-patch-let (($body
                      (el-patch-splice 2 0 ; remove dolist and binding.
                        (dolist (entry index-alist)
                          (setq imenu-list--line-entries (append imenu-list--line-entries (list entry)))
                          (imenu-list--insert-entry entry depth)
                          (when (imenu--subalist-p entry)
                            (imenu-list--insert-entries-internal (cdr entry) (1+ depth)))))))
        (el-patch-swap
          $body
          (let ((last-nested-index (imenu-list-hacks--last-nested-index index-alist)))
            (-each-indexed index-alist
              (lambda (index entry)
                (imenu-list-hacks--insert-rule-if-remaining-items-at-level-0 depth index last-nested-index)
                $body))))))))



;; Render placeholder text instead of raising user-visible errors.

(defun imenu-list-hacks--insert-placeholder-for-failed-update ()
  (with-current-buffer (imenu-list-get-buffer-create)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "No entries could be displayed" 'face 'error)))))

(with-eval-after-load 'imenu-list
  (with-no-warnings
    (el-patch-defun imenu-list-update (&optional raise-imenu-errors force-update)
      "Update the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it.
If RAISE-IMENU-ERRORS is non-nil, any errors encountered while trying to
create the index will be raised.  Otherwise, such errors will be printed
instead.
When RAISE-IMENU-ERRORS is nil, then the return value indicates if an
error has occured.  If the return value is nil, then there was no error.
Oherwise `imenu-list-update' will return the error that has occured, as
 (ERROR-SYMBOL . SIGNAL-DATA).
If FORCE-UPDATE is non-nil, the imenu-list buffer is updated even if the
imenu entries did not change since the last update."
      (catch 'index-failure
        (let ((old-entries imenu-list--imenu-entries)
              (location (point-marker)))
          ;; don't update if `point' didn't move - fixes issue #11
          (unless (and (null force-update)
                       imenu-list--last-location
                       (marker-buffer imenu-list--last-location)
                       (= location imenu-list--last-location))
            (setq imenu-list--last-location location)
            (if raise-imenu-errors
                (imenu-list-collect-entries)
              (condition-case err
                  (imenu-list-collect-entries)
                (error
                 (el-patch-remove (message "imenu-list: couldn't create index because of error: %S" err))
                 (el-patch-add
                   (imenu-list-hacks--insert-placeholder-for-failed-update)
                   (run-hooks 'imenu-list-update-hook))
                 (throw 'index-failure err))))
            (when (or force-update
                      ;; check if Ilist buffer is alive, in case it was killed
                      ;; since last update
                      (null (get-buffer imenu-list-buffer-name))
                      (not (equal old-entries imenu-list--imenu-entries)))
              (with-current-buffer (imenu-list-get-buffer-create)
                (imenu-list-insert-entries)))
            (imenu-list--show-current-entry)
            (when imenu-list-auto-resize
              (imenu-list-resize-window))
            (run-hooks 'imenu-list-update-hook)
            nil))))))

(provide 'imenu-list-hacks)

;;; imenu-list-hacks.el ends here
