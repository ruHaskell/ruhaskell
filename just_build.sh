#!/bin/bash
set -eux

# Просто собираем сайт, локально.

stack build

stack exec ruHaskell rebuild

# После этого в корне репозитория смотрим в каталог _site.
