# Release

## Instructions for Redeploying Pursuit

After making a new compiler release, do the following to redeploy Pursuit using the new compiler.

1. Submit a PR with the following changes:
    - In `pursuit.cabal`, update the version to the next release using the `X.X.X` version schema.
    - In `stack.yaml`, update `purescript` (and possibly `purescript-cst`) to use its new version.
1. Once the PR is merged, create a new GitHub tagged release using `vX.X.X` as the version schema. The release will trigger a GitHub Actions build.
1. Wait for the GitHub Actions build to finish (it builds the assets)
1. Run `./deploy/run.sh vX.X.X`, replacing `X.X.X` with the version you created.
