;;; config-ibuffer.el --- Configuration for ibuffer  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package))

(cl-eval-when (compile)
  (require 'ibuffer))

(require 'f)
(require 'general)

(autoload 'page-break-lines--update-display-tables "page-break-lines")



;; ibuffer provides an interactive buffer list.

(use-package ibuffer
  :commands (ibuffer ibuffer-forward-line ibuffer-backward-line)
  :defines (ibuffer-show-empty-filter-groups
            ibuffer-never-show-predicates)
  :general
  ("C-x C-b" #'ibuffer-other-window)
  (:keymaps 'ibuffer-mode-map :states 'motion
   "<return>" #'ibuffer-visit-buffer
   "j" #'ibuffer-forward-line
   "k" #'ibuffer-backward-line)
  :config
  (progn
    (general-setq
     ibuffer-expert t
     ibuffer-show-empty-filter-groups nil

     ibuffer-formats '((mark modified " " (mode 1 1) " " (name 35 35 :left :elide) " " filename-and-process))

     ibuffer-never-show-predicates
     (list (rx (or "*Messages*"
                   "*magit-"
                   "*git-auto-push*"
                   ".elc"
                   "magit-process"
                   "magit-diff"
                   "magit-revision"
                   "TAGS"
                   "*Backtrace*"
                   "*new*"
                   "*Org"
                   "*Flycheck error messages*"
                   "*Quail Completions*"
                   "*scratch*"
                   "*direnv*"
                   "*calc trail*"
                   "*Help*"))
           ;; Don't show roam buffers
           (lambda (it)
             (with-current-buffer it
               (when (buffer-file-name)
                 (string-match-p "/org/roam/" (buffer-file-name)))))))

    (add-hook 'ibuffer-mode-hook #'hl-line-mode)

    ;; Show icon instead of mode name.

    (with-eval-after-load 'ibuffer
      (define-ibuffer-column mode
        (:inline t)
        (with-current-buffer (current-buffer)
          (let ((icon (all-the-icons-icon-for-buffer)))
            (if (stringp icon)
                icon
              " ")))))

    ;; Dim directory part of file path.

    (with-eval-after-load 'ibuffer
      (require 'dired+)
      (define-ibuffer-column filename-and-process
        (:name "Filename/Process")
        (let ((proc (get-buffer-process buffer))
              (filename (ibuffer-make-column-filename buffer mark)))
          (if proc
              (concat (propertize (format "(%s %s)" proc (process-status proc))
                                  'font-lock-face 'italic)
                      (if (> (length filename) 0)
                          (format " %s" filename)
                        ""))
            (propertize (f-abbrev filename) 'face 'diredp-symlink)))))

    ;; Show buffer name in a consistent way.

    (with-eval-after-load 'ibuffer
      (define-ibuffer-column name
        (:inline t)
        (let ((string (buffer-name)))
          (if (not (seq-position string ?\n))
              string
            (replace-regexp-in-string
             "\n" (propertize "^J" 'font-lock-face 'escape-glyph) string)))))))

;; ibuf-ext adds a few extra features to ibuffer.

(use-package ibuf-ext
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :config
  (setq ibuffer-show-empty-filter-groups nil))

;; ibuffer-projectile teaches ibuffer how to group files by projectile project.

(use-package ibuffer-projectile
  :commands (ibuffer-projectile-set-filter-groups)
  :functions (ibuffer-do-sort-by-alphabetic)
  :custom ((ibuffer-default-sorting-mode 'major-mode)
           (ibuffer-default-sorting-reversep t))
  :preface
  (defun config-ibuffer--setup-buffer ()
    (ibuffer-projectile-set-filter-groups)
    (add-to-list 'ibuffer-filter-groups '("emacs-src" (predicate . (when (buffer-file-name)
                                                                     (s-matches? "/share/emacs" (buffer-file-name))))))

    (add-to-list 'ibuffer-filter-groups '("src" (predicate . (when (buffer-file-name)
                                                               (s-matches? (rx (or "/src/" "/lib/")) (buffer-file-name))))))
    (add-to-list 'ibuffer-filter-groups '("test" (predicate . (when (buffer-file-name)
                                                                (s-matches? "/test/" (buffer-file-name))))))



    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))

    ;; All this buffer modification will have messed up the separator
    ;; fontification, so force the display table to update now.
    (when (bound-and-true-p page-break-lines-mode)
      (page-break-lines--update-display-tables)))
  :init
  (add-hook 'ibuffer-hook #'config-ibuffer--setup-buffer)
  :config
  (general-setq ibuffer-projectile-prefix ""))

(provide 'config-ibuffer)

;;; config-ibuffer.el ends here
