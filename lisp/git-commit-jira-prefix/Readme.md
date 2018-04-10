# git-commit-jira-prefix

A helper library that automatically prepends the JIRA ticket number to your
commit messages in Magit.

It extracts the JIRA ticket number from the current branch name, and prepends
that number to each commit message.

## Why not use a git hook?

Setting the commit message in a git hook can lead to strange changes to your
commit messages in interactive rebases. Setting ticket numbers from Magit
sidesteps this problem.


## Installation

This package depends on other packages on MELPA. Make sure you have MELPA
configured as a package repository in your init.el:

``` emacs-lisp
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
```

Then, install this file with `M-x package-install-file <path-to-this-file>`.


## Configuration

Add the following to your Emacs configuration to initialise the package.

``` emacs-lisp
(autoload 'git-commit-jira-prefix-init "git-commit-jira-prefix")

(with-eval-after-load 'git-commit
  (git-commit-jira-prefix-init))
```

Alternatively, if you use `use-package`, the form below will do the right thing:

``` emacs-lisp
(use-package git-commit-jira-prefix
  :after git-commit
  :commands git-commit-jira-prefix-init
  :config (git-commit-jira-prefix-init))
```
