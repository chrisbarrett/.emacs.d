;;; org-roam-search.el --- A search interface that works better with org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-roam works best when your nodes are divided into many files, but this
;; makes the org-search functionality unsuitable. ripgrep does a better job, but
;; has the problem that it shows the raw filenames instead of the node title.

;; This implementation of search aims to surface matched text along with the
;; title of the relevant node, when available.

;;; Code:

(require 'async)
(require 'consult)
(require 'dash)
(require 'memoize)
(require 'org-roam)
(require 'org-roam-review)
(require 'pcre2el)

(defgroup org-roam-search nil
  "Node search interface for org-roam."
  :group 'productivity
  :prefix "org-roam-search-")

(defface org-roam-search-highlight
  `((t (:inherit highlight)))
  "Face for hits for a search term."
  :group 'magit-faces)

(defvar org-roam-search-title-search-byte-limit 1024)

(defun org-roam-search--replace-links-in-string (str)
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))

      ;; Replace links with their descriptions.
      (save-excursion
        (while (search-forward-regexp org-link-bracket-re nil t)
          (replace-match (match-string 2))))

      ;; Best-effort processing for remaining line-wrapped links
      (save-excursion
        (while (search-forward-regexp (rx "[[" (+? nonl) "][" (group (+? nonl)) (? "]")) nil t)
          (replace-match (match-string 1))))

      (buffer-substring (point-min) (point-max)))))

(defun org-roam-search--candidate-group (cand transform)
  "Return title for CAND or TRANSFORM the candidate."
  (let* ((line (substring cand (1+ (length (get-text-property 0 'consult--grep-file cand)))))
         (filename (get-text-property 0 'consult--grep-file cand)))
    (if transform
        (org-roam-search--replace-links-in-string line)
      (org-roam-search--format-group-title filename))))

(defun org-roam-search--lookup-title (file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file org-roam-directory) nil nil org-roam-search-title-search-byte-limit)
    (goto-char (point-min))
    (if (search-forward-regexp (rx bol "#+title:" (* space) (group (+ any)) eol))
        (match-string 1)
      file)))

(defun org-roam-search--format-group-title (file)
  (let ((title (org-roam-search--lookup-title file))
        (dir (-some->> (file-name-directory file) (string-remove-prefix "/") (string-remove-suffix "/"))))
    (if (or (null dir) (string-blank-p dir))
        title
      (format "%s > %s" dir title))))

(ignore-errors
  (memoize 'org-roam-search--format-group-title 60))

;; HACK: brutal copy-pasta to tweak two expressions in `consult--grep-format' to
;; make outputs more readable.
(defun org-roam-search--format-results (async builder)
  "Return ASYNC function highlighting grep match results.
BUILDER is the command argument builder."
  (let ((highlight))
    (lambda (action)
      (cond
       ((stringp action)
        (setq highlight (plist-get (funcall builder action) :highlight))
        (funcall async action))
       ((consp action)
        (let (result)
          (save-match-data
            (dolist (str action)
              (when (and (string-match consult--grep-match-regexp str)
                         ;; Filter out empty context lines
                         (or (/= (aref str (match-beginning 3)) ?-)
                             (/= (match-end 0) (length str))))
                (let* ((file (match-string 1 str))
                       (line (format "%4s" (match-string 2 str)))
                       (ctx (= (aref str (match-beginning 3)) ?-))
                       (sep (if ctx "-" " "))
                       (content (substring str (match-end 0)))
                       (file-len (length file))
                       (line-len (length line)))
                  (when (> (length content) consult-grep-max-columns)
                    (setq content (substring content 0 consult-grep-max-columns)))
                  (when highlight
                    (funcall highlight content))
                  (setq str (concat file sep line sep content))
                  ;; Store file name in order to avoid allocations in `consult--grep-group'
                  (add-text-properties 0 file-len `(face consult-file consult--grep-file ,file) str)
                  (put-text-property (1+ file-len) (+ 1 file-len line-len) 'face 'consult-line-number str)
                  (when ctx
                    (add-face-text-property (+ 2 file-len line-len) (length str) 'consult-grep-context 'append str))
                  (push str result)))))
          (funcall async (nreverse result))))
       (t (funcall async action))))))

;;;###autoload
(defun org-roam-search (&optional initial)
  "Search for regexp with rg in `org-roam-directory' with INITIAL input."
  (interactive)
  (let* ((default-directory org-roam-directory)
         (read-process-output-max (max read-process-output-max (* 1024 1024))))
    (consult--read
     (consult--async-command #'consult--ripgrep-builder
       (org-roam-search--format-results #'consult--ripgrep-builder)
       :file-handler t)
     :prompt "Search Roam: "
     :lookup #'consult--lookup-member
     :state (consult--grep-state)
     :initial (consult--async-split-initial initial)
     :add-history (consult--async-split-thingatpt 'symbol)
     :require-match t
     :category 'consult-grep
     :group #'org-roam-search--candidate-group
     :history '(:input consult--grep-history)
     :sort nil)))

(defvar org-roam-search-buffer-name "*org-roam-search*")
(defvar org-roam-search-tags-buffer-name "*org-roam-search-tags*")



(defun org-roam-search--highlight-matches (regexp)
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let ((transpiled-regexp (pcre-to-elisp regexp)))
        (while (search-forward-regexp transpiled-regexp nil t)
          (unless (seq-intersection (face-at-point nil t) '(magit-section-heading org-roam-review-instructions))
            (let ((overlay (make-overlay (let ((pt (match-beginning 0)))
                                           (goto-char pt)
                                           (min pt (or (car (save-match-data (bounds-of-thing-at-point 'word)))
                                                       (line-end-position))))
                                         (let ((pt (match-end 0)))
                                           (goto-char pt)
                                           (max pt (or (cdr (save-match-data (bounds-of-thing-at-point 'word)))
                                                       (line-beginning-position)))))))
              (overlay-put overlay 'face 'org-roam-search-highlight))))))))

(defun org-roam-search--match-previews (search-regexp node)
  (let ((hits))
    (save-match-data
      (with-temp-buffer
        (insert-file-contents (org-roam-node-file node))
        (let ((org-inhibit-startup t))
          (org-mode))
        (goto-char (point-min))
        (org-roam-end-of-meta-data t)
        (while (search-forward-regexp search-regexp nil t)
          (let ((hit (list :pos (match-beginning 0)
                           :olp (ignore-errors (org-get-outline-path t t))
                           :preview
                           ;; Extracted from implementation of
                           ;; `org-roam-preview-get-contents'
                           (let ((s (funcall org-roam-preview-function)))
                             (dolist (fn org-roam-preview-postprocess-functions)
                               (setq s (funcall fn s)))
                             (org-roam-fontify-like-in-org-mode s)))))
            (push hit hits)))))
    (->> (nreverse hits)
         ;; Take the first hit from each outline
         (seq-group-by (lambda (it) (plist-get it :olp)))
         (ht-from-alist)
         (ht-map (lambda (_key values) (car values))))))

(defun org-roam-search-make-insert-preview-fn (search-regexp)
  (lambda (node)
    (let ((hits-in-file (org-roam-search--match-previews search-regexp node)))
      (cond
       (hits-in-file
        (--each-indexed hits-in-file
          (magit-insert-section section (org-roam-preview-section)
            (-let [(&plist :olp :preview :pos) it]
              (when (and olp (< 1 (length olp)))
                (let ((start (point))
                      (heading (propertize (string-join olp " > ") 'face 'org-roam-title)))
                  (insert heading)
                  (fill-region start (point))
                  (insert "\n")))
              (insert preview)
              (oset section file (org-roam-node-file node))
              (oset section point pos)
              (insert "\n\n")))))
       ((string-match-p search-regexp (org-roam-node-title node))
        (insert (propertize "(Matched title)" 'font-lock-face 'font-lock-comment-face))
        (insert "\n\n"))
       (t
        (magit-cancel-section))))))

(defvar org-roam-search-view-query-history nil)

(defun org-roam-search--ripgrep-for-nodes (query)
  (let ((reporter (make-progress-reporter "Searching nodes"))
        (files (ht-create)))
    (async-wait
     (async-start-process "ripgrep" "rg"

                          (lambda (_)
                            (goto-char (point-min))
                            (while (not (eobp))
                              (progress-reporter-update reporter)
                              (-when-let* ((line (buffer-substring (line-beginning-position) (line-end-position)))

                                           ((parsed &as &plist :type)
                                            (json-parse-string line :object-type 'plist))

                                           ((&plist :data (&plist :path (&plist :text file) :absolute_offset pos))
                                            (when (equal "match" type)
                                              parsed))
                                           (file (expand-file-name file org-roam-directory)))
                                (puthash file file files))
                              (forward-line)))

                          "--smart-case"
                          "--json"
                          query org-roam-directory))
    (progress-reporter-done reporter)
    (seq-filter (lambda (node)
                  (ht-get files (org-roam-node-file node)))
                (org-roam-review-node-list))))

(defun org-roam-search--make-insert-nodes-fn (query)
  (-lambda ((&plist :nodes :placeholder :root))
    (let ((nodes (seq-remove #'org-roam-review-node-ignored-p nodes)))
      (cond
       ((null nodes)
        (insert placeholder)
        (newline))
       (t
        (pcase-dolist (`(,_file . ,group) (seq-group-by #'org-roam-node-file nodes))
          (when-let* ((top-node (-max-by (-on #'< #'org-roam-node-level)
                                         group) )
                      (node-id (org-roam-node-id top-node))
                      (heading (org-link-display-format (org-roam-node-title top-node))))
            (magit-insert-section section (org-roam-node-section node-id t)
              (magit-insert-heading
                (concat (propertize heading 'font-lock-face 'magit-section-heading)
                        " "
                        (when-let* ((mat (org-roam-review-node-maturity top-node)))
                          (alist-get mat org-roam-review-maturity-emoji-alist nil nil #'equal))))
              (oset section parent root)
              (oset section node top-node)
              (oset section washer
                    (lambda ()
                      (org-roam-review-insert-preview top-node)
                      (org-roam-search--highlight-matches query)
                      (magit-section-maybe-remove-visibility-indicator section))))))
        (org-roam-search--highlight-matches query))))))

;;;###autoload
(defun org-roam-search-view (query)
  "Search `org-roam-directory' for nodes matching a query.

QUERY is a PRCE regexp string that will be passed to ripgrep."
  (interactive (list
                (let* ((default (car org-roam-search-view-query-history))
                       (prompt (format "Search Roam%s: " (if default (format " (default \"%s\")" default) "")))
                       (input (string-trim (read-string prompt nil 'org-roam-search-view-query-history org-roam-search-view-query-history))))
                  (if (and (string-match-p (rx "|") input)
                           (not (string-prefix-p "(" input)))
                      (format "(%s)" input)
                    input))))
  (org-roam-review-display-buffer-and-select
   (org-roam-review-create-buffer
    :title (format "Search Results: %s" query)
    :placeholder "No search results"
    :buffer-name org-roam-search-buffer-name
    :insert-nodes-fn (org-roam-search--make-insert-nodes-fn query)
    :nodes
    (lambda ()
      (org-roam-search--ripgrep-for-nodes query)))))

(defvar org-roam-search-view-tags-query-history nil)

;;;###autoload
(defun org-roam-search-tags (query)
  "Search `org-roam-directory' for nodes matching a tags query.

QUERY is an `org-tags-filter'."
  (interactive (list (org-tags-filter-read "Search by tags filter (+/-): ")))
  (org-roam-review-modify-tags query t)
  (org-roam-review-display-buffer-and-select
   (org-roam-review-create-buffer
    :title "Tag Search Results"
    :instructions "The list below contains nodes matching the given tags."
    :placeholder "No search results"
    :buffer-name org-roam-search-tags-buffer-name
    :sort (-on #'string-lessp #'org-roam-node-title))))

(defun org-roam-search--kill-buffer ()
  (when-let* ((buf (get-buffer org-roam-search-buffer-name)))
    (kill-buffer buf)))

(add-hook 'org-roam-node-processed-hook #'org-roam-search--kill-buffer)

(provide 'org-roam-search)

;;; org-roam-search.el ends here
