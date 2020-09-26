#!/usr/bin/env bash

# Pursuit data backup script
# Arguments:
#  SOURCE_DIR - The directory containing all of the package data, within
#               Pursuit's data directory.
#  DEST_DIR -   A directory containing a git repository, with an upstream
#               already set up, which the data should be rsync'd to, and then
#               committed, and pushed.

set -e              # exit on error
set -u              # make undefined variables cause errors
set -o pipefail     # propagatee errors in pipelines

function die() {
    echo "$1" >&2
    exit 1
}

for required_cmd in rsync git; do
    which "$required_cmd" >/dev/null ||
        die "The program '$required_cmd' is required but could not be found."
done

if [ $# -ne 2 ]; then
    die "Usage: $0 SOURCE_DIR DEST_DIR"
fi

SOURCE_DIR="$1"
DEST_DIR="$2"

rsync --archive --verbose --delete --exclude .git "$SOURCE_DIR" "$DEST_DIR"
pushd "$DEST_DIR"

git add .           # add new files
git add --update .  # remove deleted files
git commit -m "Automated backup"
git push
