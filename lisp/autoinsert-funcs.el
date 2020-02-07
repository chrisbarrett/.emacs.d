;;; autoinsert-funcs.el --- Auto-insert features for shell scripts.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)
(require 'subr-x)

(autoload 'projectile-project-p "projectile")
(autoload 'yas-expand-snippet "yasnippet")


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



(defconst autoinsert-funcs-forms
  (list autoinsert-funcs-sh-form
        autoinsert-funcs-hs-src-form
        autoinsert-funcs-hs-test-form))

(provide 'autoinsert-funcs)

;;; autoinsert-funcs.el ends here
