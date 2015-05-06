Various architecture notes.

Uploading packages
------------------

- The package author runs psc-publish to produce some JSON, which is uploaded
  to the server, and stored in a segregated place for packages pending
  verification. This is necessary because we want to be able to associate any
  package with a github user who uploaded it (for accountability, and so that
  we can ban if necessary - fingers crossed that won't be necessary)
- The server replies with a verification URL.
- The author visits this verification URL, and is prompted to log in using
  GitHub OAuth if necessary.
- Once the author is logged in, the package is considered verified, and its
  documentation etc will now appear on pursuit.

Caching
-------

I would like to have it so that the bulk of the Yesod application knows little
or nothing about caching. We can probably arrange this by implementing a Yesod
middleware which stores each static page in Memcache or something, and whenever
a package is uploaded, all of its cached pages are invalidated (since the
version selector will need updating for all of them).

Database
--------

We only really need persistence and backup. It would also be nice to have the 
ability to just spin up more application servers if we need to.

Currently the 'database' is a TVar, which means it's not very good at any of
these things. At the moment I'm leaning towards using the filesystem instead.
Backup could be done with a periodic job which commits to a Git repository and
pushes to GitHub.
