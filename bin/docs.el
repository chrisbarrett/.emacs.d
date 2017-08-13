(defvar git-root-path
  (expand-file-name ".." (file-name-directory load-file-name)))

(defvar git-lib-file
  (expand-file-name "git.el" git-root-path))

(defvar git-readme-file
  (expand-file-name "README.md" git-root-path))

(defvar git-readme-template
  (expand-file-name "README.md.tpl" git-root-path))

(defvar git-fn-doc-mapping (make-hash-table :test 'equal))

(require 'git git-lib-file)

(-map
 (lambda (lib)
   (when (equal (car lib) git-lib-file)
     (-select
      (lambda (alist)
        (when (and
               (listp alist)
               (equal (car alist) 'defun)
               (s-matches? "^git-[^-][a-z-]+\\??$" (symbol-name (cdr alist))))
          (puthash (symbol-name (cdr alist)) (documentation (cdr alist)) git-fn-doc-mapping)))
      (cdr lib))))
 load-history)

(let ((content (f-read-text git-readme-template 'utf-8)))
  (maphash
   (lambda (fn doc)
     (setq content (s-replace (concat "{{" fn "}}") doc content)))
   git-fn-doc-mapping)
  (f-write-text content 'utf-8 git-readme-file))
