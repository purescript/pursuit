# Release

## Instructions for Redeploying Pursuit

After making a new compiler release, do the following to redeploy Pursuit using the new compiler.

1. Submit a PR with the following changes:
    - In `pursuit.cabal`, do the following:
        - update the `version` field to the next release using the `X.X.X` version schema.
        - update the `purescript` version to the next release (e.g. `== 0.15.0`)
    - In `stack.yaml`, update `purescript` to use its new version.
    - Update the `LICENSE` file by running `./license-generator/generate` which executes [`cabal-plan`](https://github.com/haskell-hvr/cabal-plan)
    - Update the CHANGELOG.md to include a new section for the new release
2. Once the PR is merged, create a new GitHub tagged release using `vX.X.X` as the version schema. The release will trigger a GitHub Actions build.
3. Wait for the GitHub Actions build to finish (it builds the assets)
4. Run `./deploy/run.sh vX.X.X`, replacing `X.X.X` with the version you created.
