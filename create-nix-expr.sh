#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cabal2nix

cabal2nix --jailbreak . | tee super-user-spark.nix

