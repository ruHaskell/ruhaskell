#!/bin/bash

# Просто собираем сайт, локально.

set -e

cabal clean && cabal configure && cabal build

./dist/build/ruHaskell/ruHaskell rebuild

# После этого в корне репозитория смотрим в каталог _site.
