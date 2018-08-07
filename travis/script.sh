#!/bin/bash

set -x

curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
stack setup
stack --version
stack install alex happy
stack --no-terminal --jobs=1 build --only-dependencies
stack --no-terminal --jobs=1 test --flag pursuit:-dev --pedantic
