#!/bin/bash
set -eux

stack build
stack exec -- ruHaskell rebuild
