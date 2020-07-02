# Simple Parallel XKCD indexer

Uses https://xkcd.com/json.html

This is a research project for [tsoding/kgbotka#233](https://github.com/tsoding/kgbotka/issues/233).

## Quick Start

```
$ cabal v2-run
```

This will download all xkcds metadatas and put them into a `database.db` SQLite database. If new comics comes out just rerun `cabal v2-run` and it will download metadata of the new comics'.

## Usage

```console
$ cabal v2-run download database.db
$ cabal v2-run search database.db compiling
```