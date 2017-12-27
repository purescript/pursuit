## <a name="submitting-packages"></a>How to submit packages

1. Put the code up on GitHub. (Currently, GitHub is the only supported hosting method. If you'd rather host your code somewhere else, please open an issue and let us know).

2. Create a git tag for the version you're releasing, if you haven't already. It is recommended to use `pulp version` to do this for you, as doing it this way will also check your package for common errors.

3. Authenticate to GitHub by running `pulp login`. (This is necessary in order for us to be able to tell who uploaded which packages).

4. Change to your project directory and run `pulp publish`. This will register your package on Bower if necessary, push commits and the relevant tag to your “origin” Git remote, and then generate your documentation and upload it to Pursuit.

   `pulp publish` also accepts a `--no-push` flag which skips the Bower registration check as well as pushing commits (this is useful for uploading other people's packages, if you ever need to do this). There is also a `--push-to` option which allows you to specify a different Git remote to push tags and commits to.


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
