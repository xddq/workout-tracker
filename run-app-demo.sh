#!/bin/bash
#
# only used for docker deployment, executed after wait-for-it.sh

# use demo env
cp env.demo .env

# creates database (required on first ever run, idempotent)
dbmate create
# runs migrations
dbmate up

echo "$(date) - Starting app"
cabal run todo-app
