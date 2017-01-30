#!/usr/bin/env bash
#
# Update a git subtree in the `lisp` directory.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

PROG=$(basename "$0")

SHORT_USAGE="$PROG PACKAGE [REMOTE-URL]"

USAGE="
NAME
        $PROG -- Update a git subtree in the 'lisp' directory.

SYNOPSIS
        $SHORT_USAGE
        $PROG [--help|-h]

DESCRIPTION

        Updates a git subtree in the 'lisp' directory, using the corresponding
        remote. The remote is assumed to be a GitHub-style USER '/' REPO pair.

        If no remote has been configured yet for the subtree, the REMOTE-URL
        argument should be supplied. The remote will be added so that it can be
        omitted in the future.

ARGUMENTS

        PACKAGE

             The name of the package to update.

        REMOTE-URL

             The URL to the git remote to pull. If the remote has not been
             configured before, it will be added.

EXAMPLE

        To update the package 'which-key' at 'lisp/emacs-which-key':

             $PROG emacs-which-key

        To update the package 'magit' and configure a corresponding remote:

             $PROG magit 'https://github.com/magit/magit.git'
"

set -e

err() {
    >&2 echo "$@"
}

if [[ "$1" =~ [-]*help ]] || [[ "$1" =~ -h ]]; then
    echo "$USAGE"
    exit 0

elif [ ${#} -lt 1 ]; then
    err "Not enough arguments. Expected 1-2, but got ${#}."
    err
    err "$SHORT_USAGE"
    err
    err "See '$PROG --help' for usage."
    exit 1

elif [ ${#} -gt 2 ]; then
    err "Too many arguments. Expected 1-2, but got ${#}."
    err
    err "$SHORT_USAGE"
    err
    err "See '$PROG --help' for usage."
    err
    exit 1

elif [ ! -d "$DIR/lisp/$1" ]; then
    err "Target subtree does not exist."
    err
    err "Directory $DIR/lisp/$1 could not be accessed."
    exit 1
fi

set -e

NAME="$1"
URL="$2"

PREFIX="lisp/$NAME"

REMOTE=$(git remote | grep "/$NAME\$") || true

if [ -z "$REMOTE" ]; then
    if [ -z "$URL" ]; then
        err "No corresponding remote has been added and no URL given."
        err
        err "Specify a remote URL explicitly."
        exit 1
    else
        REPO="$(basename "$URL")"
        REPO="${REPO%.*}"
        USER=$(basename "$(dirname "$URL")")
        REMOTE="$USER/$REPO"
    fi
fi

if [ -n "$URL" ]; then
    echo '--> Adding remote...'
    (
        set -x
        git remote add "$REMOTE" "$URL"
    )
fi

echo '--> Pulling...'
(
    set -x
    git fetch -q "$REMOTE"
    git subtree -q pull --prefix "$PREFIX" "$REMOTE" master --squash -m "Merge $REMOTE@master into $PREFIX"
)

echo '--> Recompiling'
(
    FULLPATH="$DIR/$PREFIX"
    LOAD_DIRS=$(find "$DIR/lisp" -type d -maxdepth 1 | while read -r LINE; do echo -n "-L $LINE"; done)

    # Don't actually echo the command, the load path is too long.
    echo "+ emacs -q --batch [-L PATH]* --eval '(byte-recompile-directory \"$FULLPATH\" 0)'"

    emacs -q --batch $LOAD_DIRS --eval '(byte-recompile-directory "'"$FULLPATH"'" 0)'
)
