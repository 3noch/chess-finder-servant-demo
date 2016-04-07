chess-finder-servant-demo
=========================

Demo app for [Haskell Servant](http://haskell-servant.readthedocs.org/en/stable/).

Build
-----

Install [Haskell stack](http://haskellstack.com) and run `stack setup && stack build`.

To build the GHCJS-based front-end:

  * `cd front`
  * `git clone git@github.com:mightybyte/servant-reflex.git`
  * `stack setup && make`

It takes a while.
