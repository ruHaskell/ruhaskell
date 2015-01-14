mkdir dist
set -e
ghc -threaded -o dist/pkg Setup.hs
dist/pkg configure
dist/pkg build
dist/build/ruHaskell/ruHaskell build
dist/build/ruHaskell/ruHaskell check
