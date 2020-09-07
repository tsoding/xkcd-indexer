# Simple Parallel XKCD indexer

Uses https://xkcd.com/json.html

This is a research project for [tsoding/kgbotka#233](https://github.com/tsoding/kgbotka/issues/233).

## Usage

### Cabal

Keep in mind that the code is targeting GHC 8.6.5

```console
$ cabal v2-run exe:xkcd-indexer download database.db
$ cabal v2-run exe:xkcd-indexer search database.db compiling
```

### Stack

```console
$ stack init --resolver=snapshot.yaml
$ stack build
$ stack run xkcd-indexer download database.db
$ stack run xkcd-indexer search database.db compiling
```
