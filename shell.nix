{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, QuickCheck, quickspec, stdenv
      , template-haskell
      }:
      mkDerivation {
        pname = "mlspec-helper";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base QuickCheck quickspec template-haskell
        ];
        homepage = "http://chriswarbo.net/git/mlspec-helper";
        description = "Helper modules for running MLSpec";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
