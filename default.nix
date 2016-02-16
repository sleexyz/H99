{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, QuickCheck, random, stdenv }:
      mkDerivation {
        pname = "H99";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base random ];
        testHaskellDepends = [ base QuickCheck ];
        homepage = "http://github.com/sleexyz/H99#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
