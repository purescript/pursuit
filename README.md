pursuit
=======

Search engine for PureScript functions

getting your library included
-----------------------------

* Release your PureScript library on Bower
* Make sure your released versions are tagged in git, with a tag like `v0.1.0`
* Send a pull request modifying `generator/libraries.json`, with a new object
  for your library. The object should include:
  * `gitUrl`: the git URL where your code can be cloned from,
  * `bowerName`: the name that your library has on the Bower registry.

The Pursuit data is rebuilt daily by an automated job, so your library should
appear within 24 hours. Any subsequent releases will automatically be shown on
Pursuit, as long as you remember to `git tag` them.
