{ mkDerivation, base, QuickCheck, quickspec, stdenv
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
}
