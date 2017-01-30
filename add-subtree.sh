#!/usr/bin/env bash
#
# Add a git subtree to the `lisp` directory.

PROG=$(basename "$0")

SHORT_USAGE="$PROG REMOTE-URL"

USAGE="
NAME
        $PROG -- Add a git subtree to the 'lisp' directory.

SYNOPSIS
        $SHORT
        $PROG [--help|-h]

ARGUMENTS

        REMOTE-URL

             The URL to the git remote to pull.

EXAMPLE

        Add the package 'magit' to 'lisp/magit':

             $PROG 'https://github.com/magit/magit.git'
"

set -e

err() {
    >&2 echo "$@"
}

if [[ "$1" =~ [-]*help ]] || [[ "$1" =~ -h ]]; then
    echo "$USAGE"
    exit 0

elif [ ${#} -lt 1 ]; then
    err "Not enough arguments. Expected 1, but got ${#}."
    err
    err "$SHORT_USAGE"
    err
    err "See '$PROG --help' for usage."
    exit 1

elif [ ${#} -gt 1 ]; then
    err "Too many arguments. Expected 1, but got ${#}."
    err
    err "$SHORT_USAGE"
    err
    err "See '$PROG --help' for usage."
    err
    exit 1
fi

set -e

URL="$1"

NAME="$(basename "$URL")"
NAME="${REPO%.*}"
USER=$(basename "$(dirname "$URL")")
REMOTE="$USER/$NAME"

PREFIX="lisp/$NAME"

echo '--> Adding remote...'
(
    set -x
    git remote add "$REMOTE" "$URL"
)

echo '--> Pulling...'
(
    set -x
    git fetch -q "$REMOTE"
    git subtree -q add --prefix "$PREFIX" "$REMOTE" master --squash -m "Add $REMOTE@master to $PREFIX"
)

echo '--> Compiling'
(
    FULLPATH="$DIR/$PREFIX"
    LOAD_DIRS=$(find "$DIR/lisp" -type d -maxdepth 1 | while read -r LINE; do echo -n "-L $LINE"; done)

    # Don't actually echo the command, the load path is too long.
    echo "+ emacs -q --batch [-L PATH]* --eval '(byte-recompile-directory \"$FULLPATH\" 0 t)'"

    emacs -q --batch $LOAD_DIRS --eval '(byte-recompile-directory "'"$FULLPATH"'" 0 t)'
)
