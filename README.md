![CI](https://github.com/andremarianiello/hasql-transaction-io/actions/workflows/haskell.yml/badge.svg)

### Release steps:

* Bump cabal version
* Commit changes
* hkgr tagdist # tag commit with version from cabal file
* hkgr upload # upload candidate
* hkgr publish # upload and publish package
* # hkgr upload-haddock # does not work due to solver failure. unclear why
* cabal haddock --haddock-for-hackage # build doc tarball for hackage
* cabal upload --documentation <path/to/doc/tarball> # upload doc tarball candidate
* cabal upload --documentation --publish <path/to/doc/tarball> # upload and publish doc tarball
