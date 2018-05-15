;;; imenu-list-hacks.el --- Hacky patches to imenu-list.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'dash)
(require 'el-patch)
(autoload 'imenu--subalist-p "imenu")

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

(provide 'imenu-list-hacks)

;;; imenu-list-hacks.el ends here
