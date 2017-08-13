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

Return true if there is a git repo in DIRECTORY, false otherwise.

```lisp
(git-repo? "/path/to/git/repo") ;; => t
(git-repo? "/path/to/svn/repo") ;; => nil
```

### git-branch? `(branch)`

Return true if there's a branch called BRANCH.

```lisp
(git-branch? "existing") ;; => t
(git-branch? "non-existing") ;; => nil
```

### git-tag? `(tag)`

Return true if there's a tag called TAG.

```lisp
(git-tag? "existing") ;; => t
(git-tag? "non-existing") ;; => nil
```

### git-on-branch? `(branch)`

Return true if BRANCH is currently active.

```lisp
(git-on-branch? "current") ;; => t
(git-on-branch? "other") ;; => nil
```

### git-remote? `(name)`

Return true if remote with NAME exists, false otherwise.

```lisp
(git-remote? "existing") ;; => t
(git-remote? "non-existing") ;; => nil
```

### git-on-branch `()`

Return currently active branch.

```lisp
(git-on-branch) ;; => "current-branch"
```

### git-branches `()`

List all available branches.

```lisp
(git-branches) ;; => '("master" "foo" "bar")
```

### git-log `(&optional branch)`

Log history on BRANCH.

```lisp
(git-log)
(git-log "foo")
```

### git-remotes `()`

Return list of all remotes.

```lisp
(git-remotes) ;; => '("remote-1" "remote-2")
```

### git-stashes `()`

Return list of stashes.

```lisp
(git-stashes) ;; => (:name "..." :branch "..." :message "...")
```

### git-tags `()`

Return list of all tags.

```lisp
(git-tags) ;; => '("tag-1" "tag-2")
```

### git-untracked-files `()`

Return list of untracked files.

```lisp
(git-untracked-files) ;; => '("file-1" "file-2")
```

### git-staged-files `()`

Return list of staged files.

```lisp
(git-staged-files) ;; => '("file-1" "file-2")
```

### git-add `(&rest files)`

Add PATH or everything.

```lisp
(git-add)
(git-add "foo")
(git-add "foo" "bar")
```

### git-branch `(branch)`

Create BRANCH.

```lisp
(git-branch "branch")
```

### git-checkout `(branch)`

Checkout REF.

```lisp
(git-checkout "branch")
```

### git-commit `(message &rest files)`

Commit FILES (or added files) with MESSAGE.

```lisp
(git-commit "add foo" "foo")
(git-commit "add foo and bar" "foo" "bar")
(git-commit "add em all")
```

### git-init `(&optional dir bare)`

Create new Git repo at DIR (or `git-repo').

If BARE is true, create a bare repo.

```lisp
(git-init)
(git-init "foo")
(git-init "foo" :bare)
```

### git-remote-add `(name url)`

Add remote with NAME and URL.

```lisp
(git-remote-add "foo" "foo@git.com")
```

### git-remote-remove `(name)`

Remove remote with NAME.

```lisp
(git-remote-remove "foo")
```

### git-reset `()`

Reset to COMMIT with MODE.

```lisp
(git-reset)
(git-reset "HEAD~1" 'hard)
```

### git-rm `(path)`

Remove PATH.

To remove directory, use RECURSIVE argument.

```lisp
(git-rm "foo")
(git-rm "bar" :recursive)
```

### git-stash `(&optional message)`

Stash changes in a dirty tree with MESSAGE.

If a stash was created, the name of the stash is returned,
otherwise nil is returned.

```lisp
(git-stash)
(git-stash "foo")
```

### git-stash-pop `(&optional name)`

Apply and remove stash with NAME (or first stash).

```lisp
(git-stash-pop)
(git-stash-pop "stash@{3}")
```

### git-stash-apply `(&optional name)`

Apply and keep stash with NAME (or first stash).

```lisp
(git-stash-apply)
(git-stash-apply "stash@{3}")
```

### git-tag `(tag)`

Create TAG.

```lisp
(git-tag "tag")
```

### git-clone `(url &optional dir)`

Clone URL to DIR (if present).

```lisp
(git-clone "foo@git.com")
(git-clone "foo@git.com" "bar")
```

### git-fetch `(&optional repo ref)`

Fetch REPO.

```lisp
(git-fetch)
(git-fetch "origin" "master")
```

### git-pull `(&optional repo ref)`

Pull REF from REPO.

```lisp
(git-pull)
(git-pull "origin" "master")
```

### git-push `(&optional repo ref)`

Push REF to REPO.

```lisp
(git-push)
(git-push "origin" "master")
```

### git-run `(command &rest args)`

Run git COMMAND with ARGS.

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
