{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, prim-bool
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-int";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-int";
    sha256 = "0qahfbrlzxv24s8mm8pmjggmm89ik900sa853y1nvdr8c9vs9pj8";
    rev = "708380a19c2433e3555a99304e5e127039ef2366";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat template-haskell
  ];
  testHaskellDepends = [
    base ghc-prim hedgehog prim-bool prim-compat tasty tasty-hedgehog
  ];
  homepage = "https://github.com/riz0id/prim-int";
  description = "Facilities for working with unlifted integers";
  license = lib.licenses.isc;
}
