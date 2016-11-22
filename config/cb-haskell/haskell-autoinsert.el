;;; haskell-autoinsert.el --- Autoinsert configuration for Haskell.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(autoload 'projectile-project-p "projectile")

(defconst haskell-autoinsert-forms
  '(((haskell-mode . "Haskell Src File")
     nil
     "module " (haskell-autoinsert--module-name) " where" "\n"
     "\n"
     _
     "\n")

    (("Spec\\.hs\\'" . "Haskell Test Spec")
     nil
     "module " (haskell-autoinsert--module-name) " where" "\n"
     "\n"
     "import           " (s-chop-suffix "Spec" (haskell-autoinsert--module-name)) "\n"
     "import           Test.Hspec" "\n"
     "\n"
     "main :: IO ()" "\n"
     "main = hspec spec" "\n"
     "\n"
     "spec :: Spec" "\n"
     "spec = do" "\n"
     "    describe " "\"" _ "\""
     "\n")))

(defun haskell-autoinsert--module-name ()
  (-if-let (root (and (buffer-file-name) (projectile-project-p)))

      (->> (f-no-ext (buffer-file-name))
           (s-chop-prefix root)
           (s-chop-prefixes '("app/" "src/" "test/"))
           (f-split)
           (--map (let ((x (substring it 0 1))
                        (xs (substring it 1)))
                    (concat (s-upcase x) xs)))
           (s-join "."))

    (s-upper-camel-case (file-name-base))))

;;;###autoload
(defun haskell-autoinsert-init ()
  (with-eval-after-load 'autoinsert
    (dolist (form haskell-autoinsert-forms)
      (with-no-warnings
        (add-to-list 'auto-insert-alist form)))))

(provide 'haskell-autoinsert)

;;; haskell-autoinsert.el ends here
