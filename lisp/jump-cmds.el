;;; jump-cmds.el --- Shortcut commands to go to particular locations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 'paths)
(require 's)
(require 'seq)
(require 'xref)

(autoload 'dumb-jump-go "dumb-jump")
(autoload 'dumb-jump-go-other-window "dumb-jump")
(autoload 'lsp-ui-peek-find-definitions "lsp-methods")

(autoload 'find-library-name "find-func")

(defun jump-cmds--jump-to-file (file &optional pos)
  (xref-push-marker-stack)
  (let ((buf (or (get-buffer file) (find-file-noselect file))))
    (switch-to-buffer buf)
    (when pos
      (goto-char pos))))

(defun jump-to-init-file ()
  "Open the Emacs init.el file."
  (interactive)
  (jump-cmds--jump-to-file (concat user-emacs-directory "init.el")))

(defun jump-to-host-file ()
  "Open the Emacs host config file."
  (interactive)
  (jump-cmds--jump-to-file paths-hostfile))

(defun jump-to-nix-config ()
  "Open the nix home-manager config file."
  (interactive)
  (jump-cmds--jump-to-file "~/Sync/nix/home.nix"))

(defun jump-to-personal-config ()
  "Open the personal configuration file."
  (interactive)
  (jump-cmds--jump-to-file (find-library-name "personal-config")))

(defun jump-to-messages ()
  "Open the messages buffer."
  (interactive)
  (display-buffer "*Messages*"))


;; Define command for jumping to a package usage site within config files.

(defun jump-cmds--use-package-decls (file)
  "Parse FILE for `use-package' or `require' forms.

Return an alist where the key is the feature and the value is the
position in the file."
  (with-temp-buffer
    (let (matches
          (match-refence
           (rx bol (* space)
               (group (or (and "(use-package" (+ space))
                          (and "(require" (+ space) "'")))
               (group (+ (not (any space ")")))))))
      (insert (f-read-text file))

      (goto-char (point-min))
      (while (search-forward-regexp match-refence nil t)
        (push (list (match-string-no-properties 2) (match-beginning 1))
              matches))

      (nreverse matches))))

(defun jump-cmds--package-references (files)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (file files)
      (--each (jump-cmds--use-package-decls file)
        (-let* (((pkg pos) it)
                (updated (push (list (f-abbrev file) pos) (gethash pkg ht))))
          (puthash pkg updated ht))))
    ht))

(defun jump-cmds--config-files ()
  (f-files paths-config-directory
           (lambda (f)
             (and (f-ext-p f "el")
                  (not (string-prefix-p "flycheck_" (f-base f)))))
           t))

(defun jump-cmds--read-use-package-ref (files)
  (-let* ((matches (jump-cmds--package-references files))
          (package (completing-read "Jump to package configuration: " (hash-table-keys matches) nil t)))
    (pcase (gethash package matches)
      (`(,uniq) uniq)
      (hits
       (-let* ((filenames (seq-map #'car hits))
               (file (completing-read "Select usage site: " filenames)))
         (--find (equal (car it) file) hits))))))

(defun jump-to-package-usage (file pos)
  "Go to a use-package or require reference.

FILE is the file to go to.

POS is the buffer position to go to."
  (interactive (jump-cmds--read-use-package-ref (jump-cmds--config-files)))
  (jump-cmds--jump-to-file file pos))


;; Define a command for jumping to a config file.

(defun jump-cmds--config-file-shortname (file-path)
  (string-remove-prefix "config-" (f-no-ext (f-filename file-path))))

(defun jump-cmds--read-config-file (files)
  (-let* ((lookup (--map (cons (jump-cmds--config-file-shortname it) it) files))
          (choice (completing-read "Jump to config-file: " (sort (-map #'car lookup) #'string<) nil t)))
    (alist-get choice lookup nil nil #'string=)))

(defun jump-to-config-file (file)
  "Jump to emacs.d configuration file FILE."
  (interactive (list (jump-cmds--read-config-file (jump-cmds--config-files))))
  (jump-cmds--jump-to-file file))


;; Commands for jumping around source files

(defun jump-to-definition ()
  "Go to the definition of the symbol at point."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (lsp-ui-peek-find-definitions)
    (dumb-jump-go)))

(defun jump-to-definition-other-window ()
  "Go to the definition of the symbol at point."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (display-buffer
       (save-window-excursion
         (lsp-ui-peek-find-definitions)
         (current-buffer)))
    (dumb-jump-go-other-window)))


(provide 'jump-cmds)

;;; jump-cmds.el ends here
