# Pursuit

Pursuit is the home of PureScript documentation, and will soon become a tool to
search for code (by names or types) as well.

There is currently a pre-alpha version deployed at
<http://new-pursuit.purescript.org>.

## Information for package authors

### Package badges

Pursuit can generate SVG badges for your packages, which you can put on your
project's homepage, or perhaps its GitHub readme.

The suggested markup for a badge is:

```
<a href="http://pursuit.purescript.org/packages/$PACKAGE_NAME">
  <img src="http://pursuit.purescript.org/packages/$PACKAGE_NAME/badge"
       alt="$PACKAGE_NAME on Pursuit">
  </img>
</a>
```

## Information for contributors

### Database structure

Pursuit currently uses the filesystem as a database, since it requires no setup
and it makes it easy to use Git and GitHub for backing up. The data directory
is set via an environment variable (see [Configuration](#configuration)).

The structure is as follows:

```
/
  cache/
    packages/
      purescript-prelude/
        0.1.0/
          index.html
          docs/
            Prelude/
              index.html
  verified/
    purescript-prelude/
      0.1.0.json
      0.1.1.json
```

The `cache/` directory has files that mirror the URL structure of the web
application, and contains files which do not change and may be served as-is
without forwarding the request on to the Yesod application. See Handler.Caching
for more details.

The `verified/` directory stores packages which have been verified, and
therefore appear on the site. Each package has its own directory, and then
there is a JSON file for each version. The JSON file contains a serialized
`Package GithubUser`; see Language.PureScript.Docs.Types in the compiler for
details about these types.

The backup process simply involves rsyncing everything in the `verified/`
directory into a git repository, making a commit, and pushing it to GitHub.

### Configuration

All configuration is done at startup via environment variables. The relevant
code is in the Settings module.

All configuration variable names start with `PURSUIT_` (eg,
`PURSUIT_STATIC_DIR`). Most environment variables are not required, and have
sensible defaults if not specified. The ones which _are_ required are:

* `PURSUIT_GITHUB_CLIENT_ID`: Github OAuth client id, for signing users in.
* `PURSUIT_GITHUB_CLIENT_SECRET`: Github OAuth client secret, for signing users
  in.

See `src/Settings.hs` for more details.

One way to supply the application with environment variables (if you are on a
system which uses Bash) is to use a script like the one in
`config/development.env`.
