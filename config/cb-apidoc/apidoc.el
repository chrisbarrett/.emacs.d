;;; apidoc.el --- Utilities for working with Apidoc.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)

(autoload 'json-read-from-string "json")

(defun apidoc--parse-yaml (file)
  (let ((command (concat
                  "ruby -ryaml -rjson -e 'puts JSON.pretty_generate(YAML.load(ARGF))' < "
                  (shell-quote-argument file))))
    (json-read-from-string (shell-command-to-string command))))

(defun apidoc--parse-apidoc-yaml (s)
  (-let [(&alist 'code ((org (svc)))) s]
    (list :org (symbol-name org)
          :service (symbol-name svc))))

(defun apidoc--locate-or-prompt-for-file (name)
  (-if-let* ((dir (locate-dominating-file default-directory name))
             (is-file? (file-regular-p (concat dir name))))
      (concat dir name)
    (read-file-name name nil nil nil nil #'file-exists-p)))

(defun apidoc-upload (api-json api-yaml)
  "Upload API-JSON to apidoc, parsing API-YAML for project settings."
  (interactive (list (apidoc--locate-or-prompt-for-file "api.json")
                     (apidoc--locate-or-prompt-for-file ".apidoc")))
  (-let* (((&plist :org org :service service)
           (apidoc--parse-apidoc-yaml (apidoc--parse-yaml (car (file-expand-wildcards api-yaml t)))))
          (command (format "apidoc upload %s %s %s"
                           (shell-quote-argument org)
                           (shell-quote-argument service)
                           (shell-quote-argument (car (file-expand-wildcards api-json t))))))
    (compile command t)))

(defvar apidoc-host "apidoc.movio.co")

(defun apidoc--url-for-service (host org service)
  (format "http://%s/%s/%s" host org service))

(defun apidoc-browse (api-yaml)
  "Upload API-JSON to apidoc, parsing API-YAML for project settings."
  (interactive (list (apidoc--locate-or-prompt-for-file ".apidoc")))
  (-let [(&plist :org org :service service)
         (apidoc--parse-apidoc-yaml (apidoc--parse-yaml (car (file-expand-wildcards api-yaml t))))]
    (browse-url (apidoc--url-for-service apidoc-host org service))))

(defun apidoc-update (dir)
  "Run apidoc update in DIR.  That directory should contain a .apidoc file."
  (interactive (list (file-name-directory (apidoc--locate-or-prompt-for-file ".apidoc"))))
  (let ((default-directory dir))
    (compile "apidoc update")))

(provide 'apidoc)

;;; apidoc.el ends here
