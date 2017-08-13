;;; cb-blogging.el --- Configuration for blog.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'cb-emacs)
(require 'f)
(require 'spacemacs-keys)

(with-eval-after-load 'which-key
  (with-no-warnings
    (push `(("," . ,(rx bos "cb-blogging-" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)
    (push `(("," . ,(rx bos "op/" (group (+ nonl)))) . (nil . "\\1"))
          which-key-replacement-alist)))

(use-package org-page
  :commands (op/new-post
             op/new-repository
             op/do-publication
             op/insert-options-template)
  :preface
  (progn
    (defvar op/repository-directory "~/Projects/blog")
    (defvar op/site-preview-directory "~/tmp/.op-tmp")
    (defvar cb-blogging-email "chris+blog@walrus.cool")
    (defvar op/personal-linkedin-link "https://www.linkedin.com/in/chrisdbarrett/")

    (autoload 'httpd-serve-directory "simple-httpd")

    (define-minor-mode op-blog-mode
      "Minor mode for interacting with blog files.")

    (defun cb-blogging-maybe-enable-op-blog-mode ()
      (when (and (derived-mode-p 'org-mode)
                 (buffer-file-name)
                 (f-child-of? (buffer-file-name) op/repository-directory))
        (op-blog-mode +1)))

    (defun cb-blogging-publish (path)
      (interactive (list op/site-preview-directory))
      (let ((user-mail-address cb-blogging-email))
        (op/do-publication t nil path)
        (httpd-serve-directory path)
        (browse-url (format "http://localhost:%d" httpd-port))))

    (add-hook 'org-mode-hook #'cb-blogging-maybe-enable-op-blog-mode))

  :init
  (progn
    (spacemacs-keys-declare-prefix-for-mode 'org-mode "mp" "org-page")
    (spacemacs-keys-set-leader-keys-for-minor-mode 'op-blog-mode
      "pn" #'op/new-post
      "pp" #'cb-blogging-publish
      "po" #'op/insert-options-template))

  :config
  (progn
    (setq op/theme-root-directory (concat cb-emacs-config-directory "/cb-blogging/themes/"))
    (setq op/theme 'walrus)

    (setq op/site-domain "https://walrus.cool")
    (setq op/site-main-title "Chris Barrett")
    (setq op/site-sub-title "walrus.cool")
    (setq op/personal-github-link "https://github.com/chrisbarrett")
    (setq op/personal-avatar "https://www.gravatar.com/avatar/0b3e2013f4e4f009640bc939a11dc79e?s=400")
    (setq op/hashover-comments t)

    (setq op/category-config-alist
          '(("blog" ;; this is the default configuration
             :show-meta t
             :show-comment nil
             :uri-generator op/generate-uri
             :uri-template "/blog/%y/%t/"
             :sort-by :date
             :category-index t)
            ("index"
             :show-meta nil
             :show-comment nil
             :uri-generator op/generate-uri
             :uri-template "/"
             :sort-by :date
             :category-index nil)))))

(provide 'cb-blogging)

;;; cb-blogging.el ends here
