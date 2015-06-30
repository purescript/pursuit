# Pursuit

Pursuit is the home of PureScript documentation, and will soon become a tool to
search for code (by names or types) as well.

There is currently a pre-alpha version deployed at
<http://new-pursuit.purescript.org>.

## Information for package authors

### Submitting packages

1. Put the code up on GitHub. (Currently, GitHub is the only supported hosting
   method. If you'd rather host your code somewhere else, please open an issue
   and let us know).

2. Release your package on Bower, by using `bower register`, creating a
   git tag, and pushing the tag to GitHub.

3. Ensure that the tagged version is checked out, change to your project
   directory, and run `psc-publish > pursuit.json`. This will go through your
   `bower.json` file and all of your code, collecting all of the information
   necessary to host your package on Pursuit, and dump that data to
   a new file called `pursuit.json`.

4. Send an HTTP POST request, including the JSON data as the request body, to
   http://pursuit.purescript.org/packages. For example, with `curl`:

   ```
   $ curl -X POST http://pursuit.purescript.org/packages -d @pursuit.json
   ```

   (Don't worry, this will become easier soon).

5. In the reply, there will be a URL which you should visit in your browser.

6. Log in using GitHub OAuth.

7. Press the button to confirm the package upload.

Your package, together with documentation, should now appear in Pursuit.

The purpose of the two-step uploading/verifying process is to associate a
GitHub user with each package upload.

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
  uploaded/
   5sXe_9p8XtG94D7swZuYbel66Fbgn5yoZC3N-u7vIhB14n8VYYBfEB2ySYp24PVdlEANEEKPWEF74c9y.json 
  verified/
    purescript-prelude/
      0.1.0.json
      0.1.1.json
```

There is also a `cache/` directory, but that is probably going away soon.

The `uploaded/` directory contains packages which have been uploaded and are
pending verification (that is, those between steps 5-7 in the [Submitting
packages](#submitting-packages) guide). They each get a long, random filename,
which is also used as the verification URL which they are prompted to visit
after uploading. There is a maximum of 100 packages pending upload at any given
time; if more are submitted, then the oldest is deleted. Once a package has
been verified, it is moved into the `verified/` directory.

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
`config/development.env`
