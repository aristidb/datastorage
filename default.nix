{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
with haskellPackages.override { prefFun = self : haskell.ghc763Prefs self // { binary = self.binary_0_7_1_0; }; };

cabal.mkDerivation (self: {
  pname = "datastorage";
  version = "0.1";
  src = ./.;
  buildDepends = [
    attoparsec binary byteable cryptohash dataBinaryIeee754 lens mtl
    smallcheck text transformers vector
  ];
  meta = {
    description = "Data needs to be stored more elegantly";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
  buildTools = [ cabalInstall_1_18_0_3 ];
})
