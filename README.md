# Simple Parallel XKCD indexer

Uses https://xkcd.com/json.html

This is a research project for [tsoding/kgbotka#233](https://github.com/tsoding/kgbotka/issues/233).

## Usage

```console
$ cabal v2-run exe:xkcd-indexer download database.db
$ cabal v2-run exe:xkcd-indexer search database.db compiling
```
