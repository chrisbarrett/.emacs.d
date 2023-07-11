;;; cb-lang-typescript.el --- JS/TypeScript config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'autoloads)
(require 'dash)
(require 's)

(defun cb-readonly-if-in-node-modules ()
  (when (string-match-p  "/node_modules/" default-directory)
    (read-only-mode +1)))

(add-hook 'find-file-hook #'cb-readonly-if-in-node-modules)

(use-package typescript-ts-mode
  :mode
  ("\\.[cm]?ts\\'" . typescript-ts-mode)
  :config
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode)))

(use-package js
  :mode ("\\.[cm]?jsx?\\'" . js-ts-mode)
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2)
  (js-js-tmpdir (no-littering-expand-var-file-name "js"))
  :init
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode)))

(use-package prettier
  :init
  (add-hook 'typescript-ts-mode-hook 'prettier-mode 10)
  :preface
  (define-advice prettier--find-node (:around (fn server-id) inject-via-envvar)
    (pcase server-id
      ('local
       (getenv "NIX_EMACS_NODE_PROGRAM"))
      (_
       (funcall fn server-id)))))


;;; Snippet utilities

(defcustom cb-ts-import-binding-to-module-name-alist '()
  "Map the name of a default import to a module.

Expected to be set via directory variable."
  :type '(alist :key-type string :value-type string)
  :group 'yas-funcs
  :safe (lambda (it)
          (and (listp it)
               (seq-every-p #'car #'stringp)
               (seq-every-p #'cdr #'stringp))))

(require 'dash)

(defvar yas-text)

(cl-defun cb-yas-js-module-name-for-binding (&optional (text yas-text))
  (pcase (when (stringp text)
           (string-trim text))
    ('nil "")
    ("" "")
    ((guard (assoc text cb-ts-import-binding-to-module-name-alist))
     (alist-get text cb-ts-import-binding-to-module-name-alist))
    ("VError" "verror")
    ("memoize" "lodash")
    ("_" "lodash")
    ("z" "zod")
    ("cdk" "aws-cdk-lib")
    ("dynamodb" "aws-cdk-lib/aws-dynamodb")
    ("lambda" "aws-cdk-lib/aws-lambda")

    ((guard (string-match-p "{" text))
     (-let [(_ inner) (s-match (rx "{" (* space) (+? nonl) (* space) "}") text)]
       (cb-yas-js-module-name-for-binding inner)))
    (s
     (-if-let* ((match-binding (rx (* space) "*" (+ space) "as" (+ space) (group (+ (not (any space))))))
                ((_ name) (s-match match-binding text)))
         (cb-yas-js-module-name-for-binding name)
       (downcase (s-dashed-words s))))))

(defun cb-yas-js-buffer-imports-logger-p ()
  (let ((str (buffer-substring-no-properties (point-min) (point-max))))
    (string-match-p (rx bol "import" symbol-end (+? nonl) (or "winston" "logger")) str)))

(defun cb-yas-js-inside-describe-p ()
  (save-excursion
    (search-backward-regexp (rx bol (* space) symbol-start "describe" symbol-end) nil t)))

(provide 'cb-lang-typescript)

;;; cb-lang-typescript.el ends here
