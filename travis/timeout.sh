#!/bin/bash

timeout 40m $@
ret=$?
case "$ret" in
  0)
    # continue
    ;;
  124)
    echo "Timed out while executing command: $@."
    echo "Try building again by pushing a new commit."
    exit 1
    ;;
  *)
    exit "$ret"
    ;;
esac
