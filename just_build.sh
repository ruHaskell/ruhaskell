#!/bin/bash
set -eux

stack build --test --pedantic
stack exec -- ruHaskell rebuild
