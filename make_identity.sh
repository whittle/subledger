#!/usr/bin/env bash

curl -X POST -H "Content-Type: application/json" -d \
'{
    "email":       "tester@nowhere.org",
    "description": "Integration tests for Haskell subledger library",
    "reference":   "https://github.com/whittle/subledger"
 }' \
 https://api.subledger.com/v2/identities > identity.json
