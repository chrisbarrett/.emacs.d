;;; oil.el --- Quick-and-dirty major-mode for Oil shell scripts  -*- lexical-binding: t; -*-

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
;;; Code:

(require 'flymake)
(require 'sh-script)

(defgroup oil-mode nil
  "Support for the Oil shell & programming language."
  :group 'languages
  :prefix "oil-")

(defcustom oil-builtins '("json" "source")
  "Builtin functions for the oil shell language."
  :group 'oil-mode
  :type '(repeat string))

(defcustom oil-keywords '("var" "const" "setvar" "proc" "use" "module" "try")
  "Keywords used in the oil shell language.

Specifically, those keywords in the oil shell language that are
not present in sh."
  :group 'oil-mode
  :type '(repeat string))

(defcustom oil-program "oil"
  "The path to the oil binary."
  :group 'oil-mode
  :type 'string)

(defcustom oil-sigils '("$" "@" ":" "%" "^" "&")
  "Sigils used before identifiers in the oil shell language."
  :group 'oil-mode
  :type '(repeat string))

(defconst oil-font-lock-extra-keywords
  `((,(rx-to-string `(and (not (any alnum "_" "-")) (group (or ,@oil-sigils)) (+ (any alnum "_"))))
     1 font-lock-builtin-face)
    (,(rx bol (* space) "proc" (+ space) (group (? ".") (+ (any alnum "-" "_"))))
     1 font-lock-function-name-face)
    (,(rx bol (* space) "module" (+ space) (group (+ (any alnum "-" "_"))))
     1 font-lock-type-face)
    (,(rx bol (+ space) (group "###" (* nonl)))
     1 font-lock-doc-face)))



(setf (alist-get 'oil sh-ancestor-alist) 'jsh)

(setf (alist-get 'oil sh-mode-syntax-table-input)
      `(,sh-mode-syntax-table
        ?$ "'"
        ?: "'"
        ?% "'"))

(setf (alist-get 'oil sh-shell-arg) nil)

(setf (alist-get 'oil sh-builtins) oil-builtins)

(setf (alist-get 'oil sh-test) (cons "()" 1))

(setf (alist-get 'oil sh-font-lock-keywords-var)
      (append `(sh-append sh)
              oil-font-lock-extra-keywords))

(setf (alist-get 'oil sh-assignment-regexp)
      (rx bol
          (or "var" "const") (+ space)
          (group (+ (any alnum "_" "-")))
          (* space)
          "="))

(setf (alist-get 'oil sh-leading-keywords)
      `(sh-append sh ,@oil-keywords))



(defun oil-vars-setup ()
  (setq-local sh-shell-file (executable-find "oil"))
  (setq-local sh-shell 'oil)
  (setq-local paragraph-start (rx (or (regexp page-delimiter) eol)))
  (setq-local paragraph-separate (rx (or (regexp paragraph-start) "#!/")))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip (rx (+ "#") (* space)))
  (setq-local local-abbrev-table sh-mode-abbrev-table)
  (setq-local comint-dynamic-complete-functions sh-dynamic-complete-functions)

  (setq-local sh-indent-after-if 0)
  (setq-local sh-indent-after-else 0)
  (setq-local sh-indent-after-loop-construct 0)

  ;; we can't look if previous line ended with `\'
  (setq-local comint-prompt-regexp (rx bol (* space)))
  (setq-local imenu-case-fold-search nil)
  (setq font-lock-defaults
        `((sh-font-lock-keywords
           sh-font-lock-keywords-1
           sh-font-lock-keywords-2)
          nil nil
          ((?/ . "w") (?~ . "w") (?. . "w") (?- . "w") (?_ . "w")) nil
          (font-lock-syntactic-face-function . ,#'sh-font-lock-syntactic-face-function)))
  (setq-local syntax-propertize-function #'sh-syntax-propertize-function)
  (setq-local add-log-current-defun-function #'sh-current-defun-name)
  (setq-local outline-regexp "###"))

(defconst oil--error-line-regexp
  (rx bol "'" (group (+? nonl)) "'"
      ":" (group (+ digit))
      ":" (+ space) (group (+ nonl))))

(defun oil--flymake-validate-parse-buffer ()
  (save-match-data
    (goto-char (point-min))
    (let ((diagnostics))

      (while (search-forward-regexp oil--error-line-regexp nil t)
        (let* ((file (match-string 1))
               (line (string-to-number (match-string 2)))
               (text (match-string 3))
               (col
                ;; Backtrack to recover the column from the error's ^~~~~
                ;; indicator.
                (save-excursion
                  (forward-line -1)
                  (goto-char (line-beginning-position))
                  (when (looking-at (rx bol (>= 2 space) (group "^" (* "~")) (* space) eol))
                    (goto-char (match-beginning 1))
                    (1- (current-column)))))
               (diagnostic (flymake-make-diagnostic file (cons line col) nil :error text)))
          (push diagnostic diagnostics)))

      (nreverse diagnostics))))

(cl-defun oil-flymake-validate (report-fn &rest _)
  (let ((file (buffer-file-name)))
    (when (file-exists-p file)
      (let* ((buffer (generate-new-buffer " *flymake-oil*" t))
             (proc (start-process "flymake-oil" buffer oil-program "-n" file)))
        (set-process-sentinel proc (lambda (proc _)
                                     (unwind-protect
                                         (funcall report-fn
                                                  (pcase (process-status proc)
                                                    ((or 1 126 127)
                                                     :panic)
                                                    (_
                                                     (with-current-buffer buffer
                                                       (oil--flymake-validate-parse-buffer)))))
                                       (kill-buffer buffer))))))))

;;;###autoload
(define-derived-mode oil-mode prog-mode "Shell-script"
  "Major-mode for oil shell scripts."
  (oil-vars-setup)
  (sh-set-shell "oil")
  (add-hook 'flymake-diagnostic-functions #'oil-flymake-validate nil t)
  (add-hook 'completion-at-point-functions #'sh-completion-at-point-function nil t)
  (add-hook 'syntax-propertize-extend-region-functions #'syntax-propertize-multiline 'append 'local)
  (add-hook 'completion-at-point-functions #'comint-completion-at-point nil t)
  (add-hook 'hack-local-variables-hook #'sh-after-hack-local-variables nil t))

(provide 'oil)

;;; oil.el ends here
