# servant-play
[![Haskell CI](https://github.com/hasael/servant-play/actions/workflows/servant-play-ci.yml/badge.svg)](https://github.com/hasael/servant-play/actions/workflows/servant-play-ci.yml)

HTTP Api using Servant.

A Simple implementation of a User - Transaction application.

## Test
Tests can be run with in memory db using
```
stack test
```
or a live postgresql db 

```
stack test --ta real
```

## Env
```
docker-compose up
```