(require 'f)

(defvar nvm-test/test-path
  (f-parent (f-this-file)))

(defvar nvm-test/root-path
  (f-parent nvm-test/test-path))

(defvar nvm-test/sandbox-path
  (f-expand "sandbox" nvm-test/test-path))

(defmacro with-sandbox (&rest body)
  `(with-mock
    (when (f-dir? nvm-test/sandbox-path)
      (f-delete nvm-test/sandbox-path 'force))
    (f-mkdir nvm-test/sandbox-path)
    (let ((default-directory nvm-test/sandbox-path)
          (nvm-dir "/path/to/nvm/")
          (process-environment
           '("NVM_BIN=/path/to/nvm/v0.0.1/bin"
             "NVM_PATH=/path/to/nvm/v0.0.1/lib/node"
             "PATH=/path/to/foo/bin/:/path/to/nvm/v0.0.1/bin/:/path/to/bar/bin/")))
      ,@body)))

(defun write-nvmrc (version)
  (f-write version 'utf-8 (f-expand ".nvmrc" nvm-test/sandbox-path)))

(require 'ert)
(require 'el-mock)
(require 'cl-lib)
(require 'nvm (f-expand "nvm" nvm-test/root-path))
