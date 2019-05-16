;;; autoinsert-funcs.el --- Auto-insert features for shell scripts.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'subr-x)

(autoload 'projectile-project-p "projectile")
(autoload 'yas-expand-snippet "yasnippet")


;;; Elisp

(defconst autoinsert-funcs-el-gnu-license "
\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
")

(defconst autoinsert-funcs-el-form
  '((emacs-lisp-mode . "Emacs Lisp")
    nil
    "\;;; " (file-name-nondirectory (buffer-file-name)) " --- <enter description here>  "
    "-*- lexical-binding: t; -*-" '(setq lexical-binding t) \n
    \n
    ";; Copyright (C) " (format-time-string "%Y") "  "
    (getenv "ORGANIZATION") | user-full-name                \n
    \n
    ";; Author: " user-full-name " <" user-mail-address ">" \n
    autoinsert-funcs-el-gnu-license                            \n
    ";;; Commentary:"                                       \n \n
    ";;; Code:"                                             \n \n
    _                                                       \n \n
    "\(provide '" (file-name-base) ")"                      \n \n
    "\;;; " (file-name-nondirectory (buffer-file-name)) " ends here" \n))


;;; Shell-scripts

(defun autoinsert-funcs--sh-template-string ()
  (let ((program
         (if (f-ext? (buffer-name) "zsh")
             "zsh"
           "bash")))
    (yas-expand-snippet (format "#!/usr/bin/env %s

$0
" program))))

(defconst autoinsert-funcs-sh-form
  '((sh-mode . "Shell Script") . autoinsert-funcs--sh-template-string))


;;; Haskell

(defun autoinsert-funcs-hs-module-name ()
  (-if-let (root (and (buffer-file-name) (projectile-project-p)))
      (thread-last (buffer-file-name)
        (f-no-ext)
        (s-chop-prefix root)
        (s-chop-prefixes '("app/" "src/" "test/"))
        (f-split)
        (--map (let ((x (substring it 0 1))
                     (xs (substring it 1)))
                 (concat (s-upcase x) xs)))
        (s-join "."))

    (s-upper-camel-case (file-name-base))))

(defconst autoinsert-funcs-hs-src-form
  '((haskell-mode . "Haskell Src File")
    nil
    "module " (autoinsert-funcs-hs-module-name) " where" "\n"
    "\n"
    _
    "\n"))

(defconst autoinsert-funcs-hs-test-form
  '(("Spec\\.hs\\'" . "Haskell Test Spec")
    nil
    "module " (autoinsert-funcs-hs-module-name) " where" "\n"
    "\n"
    "import           " (s-chop-suffix "Spec" (autoinsert-funcs-hs-module-name)) "\n"
    "import           Test.Hspec" "\n"
    "\n"
    "main :: IO ()" "\n"
    "main = hspec spec" "\n"
    "\n"
    "spec :: Spec" "\n"
    "spec = do" "\n"
    "    describe " "\"" _ "\""
    "\n"))


;;; HTML

(defun autoinsert-funcs-html-template-string ()
  (yas-expand-snippet (string-trim
                       "
<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\" />
    <title>$0</title>
  </head>
  <body>
  </body>
</html>

")))

(defconst autoinsert-funcs-html-form
  '((web-html-mode . "HTML") . autoinsert-funcs-html-template-string))



(defconst autoinsert-funcs-forms
  (list autoinsert-funcs-sh-form
        autoinsert-funcs-el-form
        autoinsert-funcs-hs-src-form
        autoinsert-funcs-hs-test-form
        autoinsert-funcs-html-form))

(provide 'autoinsert-funcs)

;;; autoinsert-funcs.el ends here
