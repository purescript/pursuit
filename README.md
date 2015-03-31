pursuit
=======

Search engine for PureScript functions

getting your library included
-----------------------------

* Release your PureScript library on Bower
* Make sure your released versions are tagged in git, with a tag like `v0.1.0`
* Send a pull request modifying `libraries.json`, with a new object for
  your library. The object should include:
  * `github`: an object with `user` and `repo` attributes, referencing a GitHub
    repository with your code in it (if your code's not on GitHub, please open
    an issue and let us know),
  * `bowerName`: the name that your library has on the Bower registry.

The Pursuit data is rebuilt daily by an automated job, so your library should
appear within 24 hours. Any subsequent releases will automatically be shown on
Pursuit, as long as you remember to `git tag` them.

running pursuit
---------------

Pursuit uses the GitHub API for some of the information it needs, which means
that it may come up against GitHub API rate limiting if no authentication is
used.

Pursuit supports GitHub API authentication via OAuth tokens; pass the token in
via the environment variable `GITHUB_AUTH_TOKEN`. For example, if your auth
token is stored in a file `.oauth_token`:

```
GITHUB_AUTH_TOKEN=`cat .oauth_token` cabal run
```
