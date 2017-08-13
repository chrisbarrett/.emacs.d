(require 'f)
(require 'el-mock)
(require 'cl)

(require 'git (f-expand "git" (f-parent (f-dirname (f-this-file)))))

(defvar git-sandbox-path
  (f-expand "git.el" (f-dirname (make-temp-file "git"))))

(defvar git-root-path
  (f-parent (f-dirname (f-this-file))))

(defvar git-test-path
  (f-expand "test" git-root-path))

(defvar git-vendor-path
  (f-expand "vendor" git-test-path))

(unless (require 'ert nil t)
  (require 'ert (expand-file-name "ert" git-vendor-path)))

(defmacro with-sandbox (&rest body)
  `(let ((default-directory git-sandbox-path)
         (git-repo git-sandbox-path))
     (when (f-dir? git-sandbox-path)
       (f-delete git-sandbox-path :force))
     (f-mkdir git-sandbox-path)
     ,@body))

(defmacro with-git-repo (&rest body)
  `(with-sandbox
    (git-init ,git-sandbox-path)
    (git-config "user.name" "Joe")
    (git-config "user.email" "joe@doe.com")
    ,@body))

(defmacro with-initialized-git-repo (&rest body)
  `(with-git-repo
    (f-touch "README")
    (git-add "README")
    (git-commit "Initial commit." "README")
    ,@body))
