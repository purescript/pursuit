## <a name="submitting-packages"></a>How to submit packages

Packages can only be uploaded to Pursuit if the following conditions are true:
- The repository is not a mono-repo.
- Your project must be registred in the [`purescript/registry` repo's `new-packages.json` file](https://github.com/purescript/registry/blob/master/new-packages.json). If it's not yet there, then submit a PR adding it.
- `bower install` exits successfully without any conflicts.
- `pulp build` (and if applicable `pulp test`) exits successfully without any conflicts.
- A tag points to the same commit that is currently checked out (whether by a checked out commit, branch, or tag)
- The `git` working directory is clean.

`bower` often causes problems in publishing, and this is something that will be fixed once the PureScript Registry is started. Until then, keep the following in mind:
- In your package's root directory, always start from a clean bower state by running `rm -rf bower_components/ && bower cache clean`
- If a dependency is **not** in the Bower registry (e.g. `bower info purescript-package-name` returns `ENOTFOUND`), it can be installed using the long form. For example:
    - Schema: `bower install --save <package-name>=<git-https-endpoint>#<version-range>`
    - Example: `bower install --save purescript-js-uri=https://github.com/purescript-contrib/purescript-js-uri.git#^1.0.0`
- If you run `bower install` and the command encounters a version conflict and asks which one to use for a given dependency, then your package cannot be published to Pursuit. There are two possible reasons for this:
    - one or more of your `bower.json` file's `dependencies` or `devDependencies` (if used) needs to be updated
        - Try running the following:

          ```
          # installs `ncu` globally
          npm i -g npm-check-updates
          # get list of outdated packages
          ncu -p bower
          # update versions to latest ones automatically
          ncu -u -bower
          ```

    - one or more of your dependencies (e.g. `bar`) did not update their `bower.json` file's `dependencies` field to refer to the new version of their dependency (e.g. `baz`) before publishing it (e.g. `bar`). As a result, your direct dependency, `foo@2.0.0`, may depend on `baz@2.0.0` while your other direct dependency `bar@2.0.0` still depends on `baz@1.0.0`. Bower will complain when it isn't sure whether to use `baz@1.0.0` or `baz@2.0.0`.
        - Try contacting the author of the package and ask them to update their `bower.json` file correctly and publish a new release.

1. Put the code up on GitHub. (Currently, GitHub is the only supported hosting method. If you'd rather host your code somewhere else, please open an issue and let us know).

2. Verify that `bower install`, `pulp build`, and (if applicable) `pulp test` exit successfully.

3. (Optional, highly recommended) Check that the documentation looks sensible locally before publishing by running `pulp docs -- --format html`.

4. Create a git tag for the version you're releasing, if you haven't already. It is recommended to use `pulp version` to do this for you, as doing it this way will also check your package for common errors.

5. Authenticate to GitHub by running `pulp login`. (This is necessary in order for us to be able to tell who uploaded which packages).

6. Change to your project directory and run `pulp publish`. This will the push commits and the relevant tag to your "origin" Git remote, and then generate your documentation and upload it to Pursuit.

    `pulp publish` also accepts a `--no-push` flag which skips the Bower registration check as well as pushing commits (this is useful for uploading other people's packages, if you ever need to do this). There is also a `--push-to` option which allows you to specify a different Git remote to push tags and commits to.

    **Note: If `pulp publish` fails with a `400` error, try running it a second time.** Usually, your project's documentation will be successfully published on the second run. For example:

    ```
    # tag gets pushed in first run, so we don't need it in the second run
    yes | pulp publish
    yes | pulp publish --no-push
    ```


Your package, together with documentation, should now appear in Pursuit.

## <a name="submit-automated"></a>Submitting packages from a script

You can also use Pulp to submit packages from a script. Pulp prompts for confirmation when you run `pulp publish`, so you will need to use a program like `yes` to answer affirmatively.

For example, if you want to automatically upload documentation from your Travis CI build on tags, you should add the following to your `after_script` build step:

`test -n "$TRAVIS_TAG" && ( yes | pulp publish --no-push )`

Alternatively, if you don't want to use Pulp for whatever reason, you can upload packages using Pursuit's HTTP API directly:

- Gzip the JSON output produced by `psc-publish` and save it to a file.
- If you don't already have one, get a GitHub API token by visiting <https://github.com/settings/tokens/new>. No scopes are required, since the token is only used for authentication.
- Make a POST request to https://pursuit.purescript.org/packages, with gzipped JSON as the request body, including a `Content-Encoding: gzip` header, and with your GitHub token in the Authorization header, like this: `Authorization: token {token}`.

For example, using curl:

```
curl -X POST \
  https://pursuit.purescript.org/packages \
  --data-binary @pursuit.json.gz \
  -H 'Content-Encoding: gzip' \
  -H 'Accept: application/json' \
  -H "Authorization: token $(cat my-oauth-token.txt)" \
  -v
```

If your submission is successful, Pursuit will return a 201 Created response, and the URL for your newly uploaded package will be in the Location header.

## <a name="package-deprecation"></a>How to mark package as deprecated

Package deprecation is a mechanism to tell the end users that your package is no longer supported. When package is marked as deprecated, its contents will not show up in search results on Pursuit (with the only exception of the package name itself). A package can be marked as deprecated by adding a special keyword `pursuit-deprecated` to keywords section of `bower.json` and publishing a new version of the package.

## <a name="package-badges"></a>Package badges

Pursuit can generate SVG badges for your packages, which you can put on your project's homepage, or perhaps its GitHub readme.

The suggested markup for a badge is:

```
<a href="https://pursuit.purescript.org/packages/$PACKAGE_NAME">
  <img src="https://pursuit.purescript.org/packages/$PACKAGE_NAME/badge"
       alt="$PACKAGE_NAME on Pursuit">
  </img>
</a>
```
