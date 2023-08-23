#!/bin/bash

# builds haskell image with ghc 9.2.7.0 and cabal 3.6.2.0 locally and tags it
# this image is used for deployment, we made it available via docker hub
# registry under xddqxddq/haskell:ghc-9.2.7.0-cabal-3.6.2.0
docker build -t haskell:ghc-9.2.7.0-cabal-3.6.2.0 -f ./haskell-dockerfile .
