#!/bin/bash

sh -c "cabal run app &"

while inotifywait -qq -r -e modify ./app/* ./src/* ; do killall app; sh -c "cabal run app &" ; done
