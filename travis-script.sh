#!/bin/bash

set -x

timeout 40m stack --no-terminal --jobs=1 --install-ghc build --only-dependencies
ret=$?
case "$ret" in
  0)
    # continue
    ;;
  124)
    echo "Timed out while installing dependencies."
    echo "Try building again by pushing a new commit."
    exit 1
    ;;
  *)
    echo "Failed to install dependencies; stack exited with $ret"
    exit "$ret"
    ;;
esac

stack --no-terminal -j1 build --flag pursuit:-dev --pedantic
