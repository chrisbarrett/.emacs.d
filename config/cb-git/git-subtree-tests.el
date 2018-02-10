;;; git-subtree-tests.el --- Tests for git-subtree.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'git-subtree)

(ert-deftest git-subtree-test--default-name-for-url ()
  (should (equal (git-subtree--default-name-for-url "https://github.com/foo/bar.git") "foo/bar"))
  (should (equal (git-subtree--default-name-for-url "git@github.com:foo/bar.git") "foo/bar")))

(ert-deftest git-subtree-test--guess-remotes ()
  (should (equal (git-subtree--guess-remotes "foo/bar/baz/" '("remote/bar" "remote/baz"))
                 '("remote/baz")))

  (should (equal (git-subtree--guess-remotes "foo/bar/baz" '("remote/baz" "remote/bar"))
                 '("remote/baz")))

  (should (equal (git-subtree--guess-remotes "baz" '("a/baz" "b/baz"))
                 '("a/baz" "b/baz"))))

(ert-deftest git-subtree-test--rev-for-subtree ()
  (should (equal (git-subtree--rev-for-subtree "foo") "master"))

  (let ((git-subtree-rev-alist '(("foo" . "1"))))
    (should (equal (git-subtree--rev-for-subtree "foo") "1"))))


(provide 'git-subtree-tests)

;;; git-subtree-tests.el ends here
