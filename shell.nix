with (import <nixpkgs> {}).pkgs;
let pkg = haskellPackages.callPackage
            ({ mkDerivation, aeson, base, blaze-html, clay, containers, filepath, hakyll
             , mtl, pandoc, split, string-qq, text, time, unordered-containers, stdenv
             }:
             mkDerivation {
               pname = "ruHaskell";
               version = "0.2.0.0";
               sha256 = "0";
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 aeson base blaze-html clay containers filepath hakyll mtl
                 pandoc split string-qq text time unordered-containers
               ];
               homepage = "http://ruhaskell.org";
               description = "Russian community of Haskell-developers";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
