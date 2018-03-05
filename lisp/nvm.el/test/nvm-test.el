(eval-when-compile
  (defvar nvm-dir))

(defun should-have-env (env value)
  (should (string= (getenv env) value)))

(defun should-use-version (version)
  (should-have-env "NVM_BIN" (f-join nvm-dir version "bin"))
  (should-have-env "NVM_PATH" (f-join nvm-dir version "lib" "node"))
  (should-have-env "PATH" (concat (f-full (f-join nvm-dir version "bin")) ":/path/to/foo/bin/:/path/to/bar/bin/"))
  (should (string= (car exec-path) (f-join nvm-dir version "bin"))))

(defun should-use-new-version (runtime version)
  (should-have-env "NVM_BIN" (f-join nvm-dir "versions" runtime version "bin"))
  (should-have-env "NVM_PATH" (f-join nvm-dir "versions" runtime version "lib" "node"))
  (should-have-env "PATH" (concat (f-full (f-join nvm-dir "versions" runtime version "bin")) ":/path/to/foo/bin/:/path/to/bar/bin/"))
  (should (string= (car exec-path) (f-join nvm-dir "versions" runtime version "bin"))))

(defun stub-old-tuples-for (versions)
  (let ((as-tuple (lambda (version)
                    (list version (concat "/path/to/nvm/" version)))))
    (cl-map #'list as-tuple versions)))

(defun stub-new-tuples-for (vr-tuples)
  (let ((as-tuple (lambda (vr)
                    (list (car vr) (concat "/path/to/nvm/versions/" (car (cdr vr)) "/" (car vr))))))
    (cl-map #'list as-tuple vr-tuples)))

;;;; nvm-use

(ert-deftest nvm-use-test/version-not-available ()
  (should-error
   (nvm-use "v0.10.1")))

(ert-deftest nvm-use-test/version-available-no-callback ()
  (with-sandbox
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.10.1")))
   (nvm-use "v0.10.1")
   (should-use-version "v0.10.1")))

(ert-deftest nvm-use-test/version-new-directory-style-no-callback ()
  (with-sandbox
   (stub nvm--installed-versions =>
         (stub-new-tuples-for '(("v4.0.0" "node") ("iojs-v3.3.0" "io.js"))))
   (nvm-use "v4.0.0")
   (should-use-new-version "node" "v4.0.0")))

(ert-deftest nvm-use-test/version-new-directory-iojs-style-no-callback ()
  (with-sandbox
   (stub nvm--installed-versions =>
         (stub-new-tuples-for '(("v4.0.0" "node") ("v3.3.0" "io.js"))))
   (nvm-use "v3.3.0")
   (should-use-new-version "io.js" "v3.3.0")))

(ert-deftest nvm-use-test/version-available-with-callback ()
  (with-sandbox
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2" "v0.10.1")))
   (nvm-use "v0.8.2")
   (should-use-version "v0.8.2")
   (nvm-use "v0.10.1"
            (lambda ()
              (should-use-version "v0.10.1")))
   (should-use-version "v0.8.2")))

(ert-deftest nvm-use-test/version-available-with-callback-that-errors ()
  (with-sandbox
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2" "v0.10.1")))
   (nvm-use "v0.8.2")
   (should-use-version "v0.8.2")
   (should-error
    (nvm-use "v0.10.1" (lambda () (error "BooM"))))
   (should-use-version "v0.8.2")))

(ert-deftest nvm-use-test/version-available-with-callback-that-errors-no-previous ()
  (with-sandbox
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2" "v0.10.1")))
   ;; NOTE: We are not actually testing what we say we are. It's hard
   ;; to do because the error we are expecting is a
   ;; 'wrong-type-argument error, but the callback error is the one
   ;; that is caught.
   ;;
   ;; The problem before was that if the callback fails and there is
   ;; no previous version, we previously tried to set nil as version.
   ;;
   ;; No idea how to actually test this...?
   (should-error
    (nvm-use "v0.10.1" (lambda () (error "BooM"))))))

(ert-deftest nvm-use-test/short-version ()
  (with-sandbox
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.10.1")))
   (nvm-use "0.10")
   (should-use-version "v0.10.1")))

;;;; nvm-use-for

(ert-deftest nvm-use-for-test/no-config ()
  (with-sandbox
   (should-error
    (nvm-use-for nvm-test/sandbox-path))))

(ert-deftest nvm-use-for-test/config-no-such-version ()
  (with-sandbox
   (write-nvmrc "v0.10.1")
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2")))
   (should-error
    (nvm-use-for nvm-test/sandbox-path))))

(ert-deftest nvm-use-for-test/config-no-callback ()
  (with-sandbox
   (write-nvmrc "v0.10.1")
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2" "v0.10.1")))
   (nvm-use-for nvm-test/sandbox-path)
   (should-use-version "v0.10.1")))

(ert-deftest nvm-use-for-test/config-callback ()
  (with-sandbox
   (write-nvmrc "v0.10.1")
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2" "v0.10.1")))
   (nvm-use "v0.8.2")
   (should-use-version "v0.8.2")
   (nvm-use-for nvm-test/sandbox-path
                (lambda ()
                  (should-use-version "v0.10.1")))
   (should-use-version "v0.8.2")))

(ert-deftest nvm-use-for-test/no-path ()
  (with-sandbox
   (write-nvmrc "v0.8.2")
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2")))
   (nvm-use-for)
   (should-use-version "v0.8.2")))

(ert-deftest nvm-use-for-test/short-version ()
  (with-sandbox
   (write-nvmrc "v0.10")
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2" "v0.10.1")))
   (nvm-use-for nvm-test/sandbox-path)
   (should-use-version "v0.10.1")))

(ert-deftest nvm-use-for-test/newlines ()
  (with-sandbox
   (write-nvmrc "\nv0.10\n")
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2" "v0.10.1")))
   (nvm-use-for nvm-test/sandbox-path)
   (should-use-version "v0.10.1")))

;;;; nvm--find-exact-version-for

(ert-deftest nvm--find-exact-version-for-test ()
  (with-mock
   (stub
    nvm--installed-versions =>
    (stub-old-tuples-for '("v0.8.2" "v0.8.8" "v0.6.0" "v0.10.7" "v0.10.2")))
   (should-not (nvm--find-exact-version-for "0"))
   (should-not (nvm--find-exact-version-for "v0"))
   (should-not (nvm--find-exact-version-for "0.3"))
   (should-not (nvm--find-exact-version-for "v0.6.1"))
   (should-not (nvm--find-exact-version-for "v0.6.0.1"))
   (should-not (nvm--find-exact-version-for "merry christmas"))
   (should (string= (car (nvm--find-exact-version-for "v0.6.0")) "v0.6.0"))
   (should (string= (car (nvm--find-exact-version-for "v0.8.2")) "v0.8.2"))
   (should (string= (car (nvm--find-exact-version-for "v0.10.7")) "v0.10.7"))
   (should (string= (car (nvm--find-exact-version-for "v0.6")) "v0.6.0"))
   (should (string= (car (nvm--find-exact-version-for "0.8")) "v0.8.8"))
   (should (string= (car (nvm--find-exact-version-for "v0.8")) "v0.8.8"))
   (should (string= (car (nvm--find-exact-version-for "v0.10")) "v0.10.7"))
   (should (string= (car (nvm--find-exact-version-for "0.10")) "v0.10.7"))))

;;;; nvm-use-for-buffer

(ert-deftest nvm-use-for-buffer-not-visiting ()
  (with-temp-buffer
    ;; assert that there is no error:
    (nvm-use-for-buffer)))

(ert-deftest nvm-use-for-buffer-no-nvmrc ()
  (with-sandbox
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2" "v0.10.1")))
   (with-temp-buffer
     (setq buffer-file-name (f-expand "test.js" nvm-test/sandbox-path))
     ;; can't use should-use-version since we're not moving the node
     ;; path to the front
     (let ((old-path (getenv "PATH"))
           (old-nvm-bin (getenv "NVM_BIN"))
           (old-nvm-path (getenv "NVM_PATH")))
       (nvm-use-for-buffer)
       (should-have-env "NVM_BIN" old-nvm-bin)
       (should-have-env "NVM_PATH" old-nvm-path)
       (should-have-env "PATH" old-path)))))

(ert-deftest nvm-use-for-buffer-with-nvmrc ()
  (with-sandbox
   (stub nvm--installed-versions => (stub-old-tuples-for '("v0.8.2" "v0.10.1")))
   (with-temp-buffer
     (setq buffer-file-name (f-expand "test.js" nvm-test/sandbox-path))
     (write-nvmrc "v0.10.1")
     (nvm-use-for-buffer)
     (should-use-version "v0.10.1"))))
