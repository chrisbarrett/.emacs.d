# Git.el

An Elisp API for programmatically using Git.

## Installation

Add `git` to your [Cask](https://github.com/cask/cask) file:

```lisp
(depends-on "git")
```

## API

### Predicates

* [git-repo?](#git-repo-directory) `(directory)`
* [git-branch?](#git-branch-branch) `(branch)`
* [git-tag?](#git-tag-tag) `(tag)`
* [git-on-branch?](#git-on-branch-branch) `(branch)`
* [git-remote?](#git-remote-name) `(name)`

### Info

* [git-on-branch](#git-on-branch-) `()`
* [git-branches](#git-branches-) `()`
* [git-log](#git-log-optional-branch) `(&optional branch)`
* [git-remotes](#git-remotes-) `()`
* [git-stashes](#git-stashes-) `()`
* [git-tags](#git-tags-) `()`
* [git-untracked-files](#git-untracked-files-) `()`
* [git-staged-files](#git-staged-files-) `()`

### Destructive

* [git-add](#git-add-rest-files) `(&rest files)`
* [git-branch](#git-branch-branch) `(branch)`
* [git-checkout](#git-checkout-branch) `(branch)`
* [git-commit](#git-commit-message-rest-files) `(message &rest files)`
* [git-init](#git-init-optional-dir-bare) `(&optional dir bare)`
* [git-remote-add](#git-remote-add-name-url) `(name url)`
* [git-remote-remove](#git-remote-remove-name) `(name)`
* [git-reset](#git-reset-optional-commit-mode) `(&optional commit mode)`
* [git-rm](#git-rm-path) `(path)`
* [git-stash](#git-stash-optional-message) `(&optional message)`
* [git-stash-pop](#git-stash-pop-optional-name) `(&optional name)`
* [git-stash-apply](#git-stash-apply-optional-name) `(&optional name)`
* [git-tag](#git-tag-tag) `(tag)`

## Non local

* [git-clone](#git-clone-url-optional-dir) `(url &optional dir)`
* [git-fetch](#git-fetch-optional-repo-ref) `(&optional repo ref)`
* [git-pull](#git-pull-optional-repo-ref) `(&optional repo ref)`
* [git-push](#git-push-optional-repo-ref) `(&optional repo ref)`

### Misc

* [git-run](#git-run-command-rest-args) `(command &rest args)`

## Documentation and examples

### git-repo? `(directory)`

{{git-repo?}}

```lisp
(git-repo? "/path/to/git/repo") ;; => t
(git-repo? "/path/to/svn/repo") ;; => nil
```

### git-branch? `(branch)`

{{git-branch?}}

```lisp
(git-branch? "existing") ;; => t
(git-branch? "non-existing") ;; => nil
```

### git-tag? `(tag)`

{{git-tag?}}

```lisp
(git-tag? "existing") ;; => t
(git-tag? "non-existing") ;; => nil
```

### git-on-branch? `(branch)`

{{git-on-branch?}}

```lisp
(git-on-branch? "current") ;; => t
(git-on-branch? "other") ;; => nil
```

### git-remote? `(name)`

{{git-remote?}}

```lisp
(git-remote? "existing") ;; => t
(git-remote? "non-existing") ;; => nil
```

### git-on-branch `()`

{{git-on-branch}}

```lisp
(git-on-branch) ;; => "current-branch"
```

### git-branches `()`

{{git-branches}}

```lisp
(git-branches) ;; => '("master" "foo" "bar")
```

### git-log `(&optional branch)`

{{git-log}}

```lisp
(git-log)
(git-log "foo")
```

### git-remotes `()`

{{git-remotes}}

```lisp
(git-remotes) ;; => '("remote-1" "remote-2")
```

### git-stashes `()`

{{git-stashes}}

```lisp
(git-stashes) ;; => (:name "..." :branch "..." :message "...")
```

### git-tags `()`

{{git-tags}}

```lisp
(git-tags) ;; => '("tag-1" "tag-2")
```

### git-untracked-files `()`

{{git-untracked-files}}

```lisp
(git-untracked-files) ;; => '("file-1" "file-2")
```

### git-staged-files `()`

{{git-staged-files}}

```lisp
(git-staged-files) ;; => '("file-1" "file-2")
```

### git-add `(&rest files)`

{{git-add}}

```lisp
(git-add)
(git-add "foo")
(git-add "foo" "bar")
```

### git-branch `(branch)`

{{git-branch}}

```lisp
(git-branch "branch")
```

### git-checkout `(branch)`

{{git-checkout}}

```lisp
(git-checkout "branch")
```

### git-commit `(message &rest files)`

{{git-commit}}

```lisp
(git-commit "add foo" "foo")
(git-commit "add foo and bar" "foo" "bar")
(git-commit "add em all")
```

### git-init `(&optional dir bare)`

{{git-init}}

```lisp
(git-init)
(git-init "foo")
(git-init "foo" :bare)
```

### git-remote-add `(name url)`

{{git-remote-add}}

```lisp
(git-remote-add "foo" "foo@git.com")
```

### git-remote-remove `(name)`

{{git-remote-remove}}

```lisp
(git-remote-remove "foo")
```

### git-reset `()`

{{git-reset}}

```lisp
(git-reset)
(git-reset "HEAD~1" 'hard)
```

### git-rm `(path)`

{{git-rm}}

```lisp
(git-rm "foo")
(git-rm "bar" :recursive)
```

### git-stash `(&optional message)`

{{git-stash}}

```lisp
(git-stash)
(git-stash "foo")
```

### git-stash-pop `(&optional name)`

{{git-stash-pop}}

```lisp
(git-stash-pop)
(git-stash-pop "stash@{3}")
```

### git-stash-apply `(&optional name)`

{{git-stash-apply}}

```lisp
(git-stash-apply)
(git-stash-apply "stash@{3}")
```

### git-tag `(tag)`

{{git-tag}}

```lisp
(git-tag "tag")
```

### git-clone `(url &optional dir)`

{{git-clone}}

```lisp
(git-clone "foo@git.com")
(git-clone "foo@git.com" "bar")
```

### git-fetch `(&optional repo ref)`

{{git-fetch}}

```lisp
(git-fetch)
(git-fetch "origin" "master")
```

### git-pull `(&optional repo ref)`

{{git-pull}}

```lisp
(git-pull)
(git-pull "origin" "master")
```

### git-push `(&optional repo ref)`

{{git-push}}

```lisp
(git-push)
(git-push "origin" "master")
```

### git-run `(command &rest args)`

{{git-run}}

```lisp
(git-run "log")
(git-run "log" "--name-only")
;; ...
```

## Notes

For each command, you can add arguments using `git-args`.

```lisp
(let ((git-args "--hard"))
  (git-reset "HEAD"))
```

## Contribution

Be sure to!

Install [Cask](https://github.com/cask/cask) if you haven't
already.

Run the unit tests with:

    $ make test

Do not change `README.md` directly. If you want to change the README
or if you change any function comments, update the README with:

    $ make docs
