# Pursuit

[![Build Status](https://api.travis-ci.org/purescript/pursuit.svg?branch=master)](http://travis-ci.org/purescript/pursuit)

Pursuit is a web application which hosts documentation for PureScript packages,
and lets you search for code by names or types.

Pursuit is currently deployed at <http://pursuit.purescript.org>.

Information for package authors can be found at
<http://pursuit.purescript.org/help>.

## Development

It's recommended to use `stack`: <http://docs.haskellstack.org>.

To build in development mode:

```
$ stack build
```

To run the server:

```
$ stack exec pursuit
```

If you have content in your data directory
(see [Database structure](#database-structure)) then the database will
be regenerated before the server starts listening - this can take a short time
depending on how much data you have.  The site should then be available at
`http://localhost:3000`.

To build in production mode:

```
$ stack build --flag pursuit:-dev
```

## Database structure

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

The `verified/` directory stores uploaded packages.  Each package has its own
directory, and then there is a JSON file for each version. These JSON files
each contain a serialized `Package GithubUser`; see
Language.PureScript.Docs.Types in the compiler for details about these types.

The backup process simply involves rsyncing everything in the `verified/`
directory into a git repository, making a commit, and pushing it to GitHub.

## Database setup

If you need some sample packages to work with, you can clone the
[pursuit-backups][pursuit-backups] repo and copy the packages you want to the
`verified/` directory. This is more convenient than manually uploading each
package.

[pursuit-backups]: https://github.com/purescript/pursuit-backups

## Configuration

All configuration is done at startup via environment variables. The relevant
code is in the Settings module.

All configuration variable names start with `PURSUIT_` (eg,
`PURSUIT_STATIC_DIR`). All configuration variables are optional; for
development, it is fine to just run `stack exec pursuit` leaving them all
unset.
  in.

See `src/Settings.hs` for more details.
