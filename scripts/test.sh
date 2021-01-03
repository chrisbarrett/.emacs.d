#!/usr/bin/env bash
# shellcheck disable=SC2039
BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

if [ -z "$1" ]; then
	exit 1
fi

EMACS="$1"

run_emacs() {
	"$EMACS" --batch -l "$BASE/early-init.el" -l "$BASE/init.el" "$@" 2>&1
}

success() {
	printf "$GREEN"
	echo "OK"
	printf "$NC"
}

fail() {
	printf "$RED"
	echo "FAIL"
	printf "$NC"

	if [ -n "$@" ]; then
		echo "Output:"
		echo "----------------"
		echo "$@"
		echo "----------------"
	fi
}

echo -n "check: config.el loads..."
RESULT=$(run_emacs)
if echo "$RESULT" | grep 'config.el loaded' >/dev/null; then
	success
else
	fail "$RESULT"
	exit 1
fi

echo -n "check: config.el loads in less than 1 s... "
RESULT=$(run_emacs --eval '(message "%s" (floor emacs-init-duration))')
SECONDS=$(echo "$RESULT" | tail -n 1)
if [ 0 -eq "$SECONDS" ]; then
	success
else
	fail "$RESULT"
fi

echo -n "check: source directory exists... "
RESULT=$(run_emacs --eval '(message "%s" find-function-C-source-directory)')
SRC_DIR=$(echo "$RESULT" | tail -n 1)
if [ -d "$SRC_DIR" ] && [ -f "$SRC_DIR/emacs.c" ]; then
	success
else
	fail "$RESULT"
fi
