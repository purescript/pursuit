#! /usr/bin/env bash

set -ex

# This script can be run to deploy a new version of Pursuit.

pursuit_version="$1"

if [ "$pursuit_version" = "" ]
then
  echo >&2 "Need to provide a version"
  exit 1
fi

deploy_script="deploy-pursuit.sh"
scp deploy/remote.sh "root@pursuit.purescript.org:${deploy_script}"
ssh root@pursuit.purescript.org "bash ${deploy_script} ${pursuit_version}"
