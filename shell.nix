{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  super-user-spark = haskellPackages.callPackage ./super-user-spark.nix {};

in
  pkgs.stdenv.mkDerivation {
    name = "super-user-spark-env";
    buildInputs = [ pkgs.zlib super-user-spark ];
  }
