;;;; git-init

(ert-deftest git-init-test/with-dir ()
  (with-sandbox
   (git-init git-sandbox-path)
   (should (git-repo? git-sandbox-path))))

(ert-deftest git-init-test/without-dir ()
  (with-sandbox
   (let ((git-repo git-sandbox-path))
     (git-init)
     (should (git-repo? git-sandbox-path)))))

(ert-deftest git-init-test/bare ()
  (with-sandbox
   (git-init git-sandbox-path :bare)
   (should (git-repo? git-sandbox-path))))

(ert-deftest git-init-test/without-git-repo ()
  (with-sandbox
   (let (git-repo)
     (git-init git-sandbox-path :bare))
   (should (git-repo? git-sandbox-path))))


;;;; git-untracked-files

(ert-deftest git-untracked-files-test/no-files ()
  (with-git-repo
   (should-not (git-untracked-files))))

(ert-deftest git-untracked-files-test/single-file ()
  (with-git-repo
   (f-touch "foo")
   (should (equal (git-untracked-files) '("foo")))))

(ert-deftest git-untracked-files-test/multiple-files ()
  (with-git-repo
   (f-touch "foo")
   (f-touch "bar")
   (should (equal (git-untracked-files) '("bar" "foo")))))

(ert-deftest git-untracked-files-test/file-in-directory ()
  (with-git-repo
   (f-mkdir "foo")
   (f-touch (f-join "foo" "bar"))
   (should (equal (git-untracked-files) '("foo/bar")))))

(ert-deftest git-untracked-files-test/with-staged-files ()
  (with-git-repo
   (f-mkdir "foo")
   (f-touch (f-join "foo" "bar"))
   (f-touch (f-join "foo" "baz"))
   (git-add (f-join "foo" "bar"))
   (should (equal (git-untracked-files) '("foo/baz")))))


;;;; git-staged-files

(ert-deftest git-staged-files-test/no-files ()
  (with-git-repo
   (should-not (git-staged-files))))

(ert-deftest git-staged-files-test/single-file ()
  (with-git-repo
   (f-touch "foo")
   (git-add "foo")
   (should (equal (git-staged-files) '("foo")))))

(ert-deftest git-staged-files-test/multiple-files ()
  (with-git-repo
   (f-touch "foo")
   (f-touch "bar")
   (git-add "foo")
   (git-add "bar")
   (should (equal (git-staged-files) '("bar" "foo")))))

(ert-deftest git-staged-files-test/file-in-directory ()
  (with-git-repo
   (f-mkdir "foo")
   (f-touch (f-join "foo" "bar"))
   (git-add (f-join "foo" "bar"))
   (should (equal (git-staged-files) '("foo/bar")))))

(ert-deftest git-staged-files-test/with-untracked-files ()
  (with-git-repo
   (f-mkdir "foo")
   (f-touch (f-join "foo" "bar"))
   (f-touch (f-join "foo" "baz"))
   (git-add (f-join "foo" "bar"))
   (should (equal (git-staged-files) '("foo/bar")))))


;;;; git-run

(ert-deftest git-run-test/git-args ()
  (with-initialized-git-repo
   (let ((git-args '("-b" "foo")))
     (git-run "checkout"))
   (should (equal (git-on-branch) "foo"))))


;;;; git-repo?

(ert-deftest git-repo?-test/is-repo ()
  (with-git-repo
   (should (git-repo? git-sandbox-path))))

(ert-deftest git-repo?-test/is-repo-bare ()
  (with-sandbox
   (git-init git-sandbox-path :bare)
   (should (git-repo? git-sandbox-path))))

(ert-deftest git-repo?-test/is-not-repo ()
  (with-sandbox
   (should-not (git-repo? git-sandbox-path))))


;;;; git-branch?

(ert-deftest git-branch?-test/not-initialized ()
  (with-git-repo
   (should-not (git-branch? "foo"))))

(ert-deftest git-branch?-test/does-not-exist ()
  (with-initialized-git-repo
   (should-not (git-branch? "foo"))))

(ert-deftest git-branch?-test/exists ()
  (with-initialized-git-repo
   (git-branch "foo")
   (should (git-branch? "foo"))))


;;;; git-tag?

(ert-deftest git-tag?-test/not-initialized ()
  (with-git-repo
   (should-not (git-tag? "foo"))))

(ert-deftest git-tag?-test/does-not-exist ()
  (with-initialized-git-repo
   (should-not (git-tag? "foo"))))

(ert-deftest git-tag?-test/exists ()
  (with-initialized-git-repo
   (git-tag "foo")
   (should (git-tag? "foo"))))


;;;; git-on-branch

(ert-deftest git-on-branch-test/no-branch ()
  (with-git-repo
   (should-error (git-on-branch))))

(ert-deftest git-on-branch-test/master ()
  (with-initialized-git-repo
   (should (equal (git-on-branch) "master"))))

(ert-deftest git-on-branch-test/other ()
  (with-initialized-git-repo
   (git-branch "foo")
   (git-checkout "foo")
   (should (equal (git-on-branch) "foo"))))


;;;; git-on-branch?

(ert-deftest git-on-branch?-test/on-that-branch ()
  (with-initialized-git-repo
   (should (git-on-branch? "master"))))

(ert-deftest git-on-branch?-test/on-other-branch ()
  (with-initialized-git-repo
   (git-branch "foo")
   (git-checkout "foo")
   (should-not (git-on-branch? "master"))))


;;;; git-add

(ert-deftest git-add-test/single-file ()
  (with-git-repo
   (f-touch "foo")
   (git-add "foo")
   (should (equal (git-staged-files) '("foo")))))

(ert-deftest git-add-test/multiple-file ()
  (with-git-repo
   (f-touch "foo")
   (f-touch "bar")
   (git-add "foo" "bar")
   (should (equal (git-staged-files) '("bar" "foo")))))

(ert-deftest git-add-test/directory ()
  (with-git-repo
   (f-mkdir "foo")
   (f-touch (f-join "foo" "bar"))
   (git-add (f-join "foo" "bar"))
   (should (equal (git-staged-files) '("foo/bar")))))

(ert-deftest git-add-test/all ()
  (with-git-repo
   (f-mkdir "foo")
   (f-touch (f-join "foo" "bar"))
   (f-touch "bar")
   (git-add)
   (should (equal (git-staged-files) '("bar" "foo/bar")))))


;;;; git-branch/git-branches

(ert-deftest git-branch-test/not-initialized ()
  (with-git-repo
   (should-not (git-branches))))

(ert-deftest git-branch-test/master-only ()
  (with-initialized-git-repo
   (should (equal (git-branches) '("master")))))

(ert-deftest git-branch-test/single ()
  (with-initialized-git-repo
   (git-branch "foo")
   (should (equal (git-branches) '("foo" "master")))))

(ert-deftest git-branch-test/multiple ()
  (with-initialized-git-repo
   (git-branch "foo")
   (git-branch "bar")
   (should (equal (git-branches) '("bar" "foo" "master")))))

(ert-deftest git-branch-test/already-exists ()
  (with-initialized-git-repo
   (git-branch "foo")
   (should-error
    (git-branch "foo"))
   (should (equal (git-branches) '("foo" "master")))))


;;;; git-checkout

(ert-deftest git-checkout-test/does-not-exist ()
  (with-initialized-git-repo
   (should-error
    (git-checkout "foo"))))

(ert-deftest git-checkout-test/branch ()
  (with-initialized-git-repo
   (git-branch "foo")
   (git-checkout "foo")
   (should (equal (git-on-branch) "foo"))))

(ert-deftest git-checkout-test/tag ()
  (with-initialized-git-repo
   (git-tag "bar")
   (f-touch "foo")
   (git-add "foo")
   (git-commit "add foo" "foo")
   (git-checkout "bar")
   (should-not (equal (plist-get (car (git-log)) :message) "add foo"))))

(ert-deftest git-checkout-test/commit ()
  (with-initialized-git-repo
   (let ((commit (plist-get (car (git-log)) :commit)))
     (f-touch "foo")
     (git-add "foo")
     (git-commit "add foo" "foo")
     (git-checkout commit)
     (should-not (equal (plist-get (car (git-log)) :message) "add foo")))))

(ert-deftest git-checkout-test/commit ()
  (with-initialized-git-repo
   (let ((commit (plist-get (car (git-log)) :commit)))
     (f-write-text "FOO" 'utf-8 "foo")
     (git-add "foo")
     (git-commit "add foo" "foo")
     (should (equal (f-read-text "foo" 'utf-8) "FOO"))
     (f-write-text "BAR" 'utf-8 "foo")
     (should (equal (f-read-text "foo" 'utf-8) "BAR"))
     (git-checkout "foo")
     (should (equal (f-read-text "foo" 'utf-8) "FOO")))))


;;;; git-clone

(ert-deftest git-clone-test/ ()
  "Hard to test..."
  (with-mock
   (mock (git-run "clone" "url" "dir"))
   (git-clone "url" "dir")))


;;;; git-commit

(ert-deftest git-commit-test/single-file ()
  (with-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-commit "add foo" "foo")
   (should (equal (plist-get (car (git-log)) :message) "add foo"))))

(ert-deftest git-commit-test/multiple-files ()
  (with-git-repo
   (f-touch "foo")
   (f-touch "bar")
   (git-add "foo")
   (git-add "bar")
   (git-commit "add foo and bar" "foo" "bar")
   (should (equal (plist-get (car (git-log)) :message) "add foo and bar"))))

(ert-deftest git-commit-test/directory ()
  (with-git-repo
   (f-mkdir "foo")
   (f-touch (f-join "foo" "bar"))
   (git-add "foo")
   (git-commit "add foo" "foo")
   (should (equal (plist-get (car (git-log)) :message) "add foo"))))

(ert-deftest git-commit-test/all ()
  (with-git-repo
   (f-mkdir "foo")
   (f-touch (f-join "foo" "bar"))
   (git-add "foo")
   (f-touch "baz")
   (git-commit "add foo and baz")
   (git-add "baz")
   (should (equal (plist-get (car (git-log)) :message) "add foo and baz"))))

(ert-deftest git-commit-test/no-add ()
  (with-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-commit "add foo" "foo")
   (f-write-text "FOO" 'utf-8 "foo")
   (git-commit "change foo")
   (should (equal (plist-get (-first-item (git-log)) :message) "change foo"))
   (should (equal (plist-get (-last-item (git-log)) :message) "add foo"))))


;;;; git-diff

;; Todo


;;;; git-fetch

(ert-deftest git-fetch-test/no-repo ()
  "Hard to test..."
  (with-mock
   (mock (git-run "fetch" nil))
   (git-fetch)))

(ert-deftest git-fetch-test/with-repo ()
  "Hard to test..."
  (with-mock
   (mock (git-run "fetch" "repo"))
   (git-fetch "repo")))


;;;; git-log

(ert-deftest git-log-test/single-commit ()
  (with-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-commit "add foo" "foo")
   (let ((log (-first-item (git-log))))
     (should (plist-get log :commit))
     (should (equal (plist-get log :author-name) "Joe"))
     (should (equal (plist-get log :author-email) "joe@doe.com"))
     (should (equal (plist-get log :comitter-name) "Joe"))
     (should (plist-get log :date))
     (should (equal (plist-get log :message) "add foo")))))

(ert-deftest git-log-test/multiple-commits ()
  (with-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-commit "add foo" "foo")
   (f-touch "bar")
   (git-add "bar")
   (git-commit "add bar" "bar")
   (let ((log (-first-item (git-log))))
     (should (plist-get log :commit))
     (should (equal (plist-get log :author-name) "Joe"))
     (should (equal (plist-get log :author-email) "joe@doe.com"))
     (should (equal (plist-get log :comitter-name) "Joe"))
     (should (plist-get log :date))
     (should (equal (plist-get log :message) "add foo")))))

(ert-deftest git-log-test/multiple-commits ()
  (with-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-commit "add foo" "foo")
   (f-touch "bar")
   (git-add "bar")
   (git-commit "add bar" "bar")
   (let ((log (-first-item (git-log))))
     (should (plist-get log :commit))
     (should (equal (plist-get log :author-name) "Joe"))
     (should (equal (plist-get log :author-email) "joe@doe.com"))
     (should (equal (plist-get log :comitter-name) "Joe"))
     (should (plist-get log :date))
     (should (equal (plist-get log :message) "add bar")))
   (let ((log (-last-item (git-log))))
     (should (plist-get log :commit))
     (should (equal (plist-get log :author-name) "Joe"))
     (should (equal (plist-get log :author-email) "joe@doe.com"))
     (should (equal (plist-get log :comitter-name) "Joe"))
     (should (plist-get log :date))
     (should (equal (plist-get log :message) "add foo")))))


;;;; git-pull

(ert-deftest git-pull-test/ ()
  (with-mock
   (mock (git-run "pull" "origin" "master"))
   (git-pull "origin" "master")))


;;;; git-push

(ert-deftest git-push-test/ ()
  (with-mock
   (mock (git-run "push" "origin" "master"))
   (git-push "origin" "master")))


;;;; git-remotes/git-remote-add

(ert-deftest git-remotes-test/no-remote ()
  (with-git-repo
   (should-not (git-remotes))))

(ert-deftest git-remotes-test/single-remote ()
  (with-git-repo
   (git-remote-add "foo" "bar")
   (should (equal (git-remotes) '("foo")))))

(ert-deftest git-remotes-test/multiple-remotes ()
  (with-git-repo
   (git-remote-add "foo" "bar")
   (git-remote-add "baz" "qux")
   (should (equal (git-remotes) '("baz" "foo")))))


;;;; git-remote-remove

(ert-deftest git-remote-remove-test/does-not-exist ()
  (with-git-repo
   (should-error (git-remote-remove "foo"))))

(ert-deftest git-remote-remove-test/exists ()
  (with-git-repo
   (git-remote-add "foo" "bar")
   (git-remote-add "baz" "qux")
   (git-remote-remove "foo")
   (should (equal (git-remotes) '("baz")))))


;;;; git-remote?

(ert-deftest git-remote?-test/does-not-exist ()
  (with-git-repo
   (should-not (git-remote? "foo"))))

(ert-deftest git-remote?-test/exists ()
  (with-git-repo
   (git-remote-add "foo" "bar")
   (should (git-remote? "foo"))))


;;;; git-reset

(ert-deftest git-reset-test/default-mixed ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (f-touch "bar")
   (should (equal (git-staged-files) '("foo")))
   (git-reset)
   (should-not (git-staged-files))))

(ert-deftest git-reset-test/soft ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-commit "add foo" "foo")
   (f-touch "bar")
   (f-touch "baz")
   (git-add "baz")
   (should (equal (length (git-log)) 2))
   (git-reset "HEAD~1" 'soft)
   (should (equal (length (git-log)) 1))))

(ert-deftest git-reset-test/hard ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (f-touch "bar")
   (git-reset "HEAD" 'hard)
   (should-not (f-file? "foo"))
   (should (f-file? "bar"))))


;;;; git-rm

(ert-deftest git-rm-test/file ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-commit "add foo" "foo")
   (git-rm "foo")
   (should-not (f-file? "foo"))
   (should (equal (git-staged-files) '("foo")))))

(ert-deftest git-rm-test/dir ()
  (with-initialized-git-repo
   (f-mkdir "foo")
   (f-touch (f-join "foo" "bar"))
   (git-add "foo")
   (git-commit "add foo" "foo")
   (git-rm "foo" :recursive)
   (should (equal (git-staged-files) '("foo/bar")))))


;;;; git-stash/git-stashes

(ert-deftest git-stash-test/no-stashes ()
  (with-initialized-git-repo
   (should-not (git-stashes))))

(ert-deftest git-stash-test/no-message ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (should (equal (git-stash) "stash@{0}"))
   (let ((stash (car (git-stashes))))
     (should (equal (plist-get stash :name) "stash@{0}"))
     (should (equal (plist-get stash :branch) "master"))
     (should (s-matches? "[a-z0-9]\\{7\\} Initial commit." (plist-get stash :message))))))

(ert-deftest git-stash-test/with-message ()
  (with-initialized-git-repo
   (f-touch "foo")
   (f-touch "bar")
   (git-add "bar")
   (should (equal (git-stash "baz") "stash@{0}"))
   (let ((stash (car (git-stashes))))
     (should (equal stash '(:name "stash@{0}" :branch "master" :message "baz"))))))

(ert-deftest git-stash-test/multiple-stashes ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-stash "first")
   (f-touch "bar")
   (git-add "bar")
   (should (equal (git-stash "second") "stash@{0}"))
   (should (equal (git-stashes)
                  '((:name "stash@{0}" :branch "master" :message "second")
                    (:name "stash@{1}" :branch "master" :message "first"))))))

(ert-deftest git-stash-test/other-branch ()
  (with-initialized-git-repo
   (git-branch "foo")
   (git-checkout "foo")
   (f-touch "bar")
   (git-add "bar")
   (should (equal (git-stash "baz") "stash@{0}"))
   (let ((stash (car (git-stashes))))
     (should (equal stash '(:name "stash@{0}" :branch "foo" :message "baz"))))))

(ert-deftest git-stash-test/no-changes ()
  (with-initialized-git-repo
   (f-touch "foo")
   (should-not (git-stash "bar"))))


;;;; git-stash-pop

(ert-deftest git-stash-pop-test/single-stash ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-stash "bar")
   (should-not (git-staged-files))
   (git-stash-pop)
   (should (equal (git-staged-files) '("foo")))
   (should-not (git-stashes))))

(ert-deftest git-stash-pop-test/multiple-stashes ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-stash "first")
   (f-touch "bar")
   (git-add "bar")
   (git-stash "second")
   (should-not (git-staged-files))
   (git-stash-pop)
   (should (equal (git-staged-files) '("bar")))
   (should (equal (git-stashes) '((:name "stash@{0}" :branch "master" :message "first"))))))

(ert-deftest git-stash-pop-test/by-name ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-stash "first")
   (f-touch "bar")
   (git-add "bar")
   (git-stash "second")
   (should-not (git-staged-files))
   (git-stash-pop "stash@{1}")
   (should (equal (git-staged-files) '("foo")))
   (should (equal (git-stashes) '((:name "stash@{0}" :branch "master" :message "second"))))))


;;;; git-stash-apply

(ert-deftest git-stash-apply-test/single-stash ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-stash "bar")
   (should-not (git-staged-files))
   (git-stash-apply)
   (should (equal (git-staged-files) '("foo")))
   (should (equal (git-stashes) '((:name "stash@{0}" :branch "master" :message "bar"))))))

(ert-deftest git-stash-apply-test/multiple-stashes ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-stash "first")
   (f-touch "bar")
   (git-add "bar")
   (git-stash "second")
   (should-not (git-staged-files))
   (git-stash-apply)
   (should (equal (git-staged-files) '("bar")))
   (should (equal (git-stashes) '((:name "stash@{0}" :branch "master" :message "second")
                                  (:name "stash@{1}" :branch "master" :message "first"))))))

(ert-deftest git-stash-apply-test/by-name ()
  (with-initialized-git-repo
   (f-touch "foo")
   (git-add "foo")
   (git-stash "first")
   (f-touch "bar")
   (git-add "bar")
   (git-stash "second")
   (should-not (git-staged-files))
   (git-stash-apply "stash@{1}")
   (should (equal (git-staged-files) '("foo")))
   (should (equal (git-stashes) '((:name "stash@{0}" :branch "master" :message "second")
                                  (:name "stash@{1}" :branch "master" :message "first"))))))


;;;; git-tag/git-tags

(ert-deftest git-tag-test/no-tags ()
  (with-initialized-git-repo
   (should-not (git-tags))))

(ert-deftest git-tag-test/single-tag ()
  (with-initialized-git-repo
   (git-tag "foo")
   (should (equal (git-tags) '("foo")))))

(ert-deftest git-tag-test/single-tag ()
  (with-initialized-git-repo
   (git-tag "foo")
   (git-tag "bar")
   (should (equal (git-tags) '("bar" "foo")))))

(ert-deftest git-tag-test/already-existing ()
  (with-initialized-git-repo
   (git-tag "foo")
   (should-error
    (git-tag "foo"))
   (should (equal (git-tags) '("foo")))))


;;;; git-config

(ert-deftest git-config-test/does-not-exist ()
  (with-initialized-git-repo
   (should-not (git-config "user.foo"))))

(ert-deftest git-config-test/exists ()
  (with-initialized-git-repo
   (git-config "user.foo" "bar")
   (should (equal (git-config "user.foo") "bar"))))
