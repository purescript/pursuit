#!/bin/bash

set -ex

curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
stack setup
stack --version

if [ -n "$TRAVIS_TAG" ]
then
  STACK_EXTRA_FLAGS="--flag pursuit:-dev"
else
  STACK_EXTRA_FLAGS="--fast"
fi

stack --no-terminal --jobs=2 build --only-dependencies $STACK_EXTRA_FLAGS
stack --no-terminal --jobs=2 test --pedantic $STACK_EXTRA_FLAGS
