{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
with haskellPackages_ghc782;

cabal.mkDerivation (self: {
  pname = "datastorage";
  version = "0.1";
  src = ./.;
  buildDepends = [
    attoparsec binary byteable conduitCombinators cryptohash
    dataBinaryIeee754 deepseq exceptions hashable lens mmorph mtl
    smallcheck text transformers unorderedContainers vector QuickCheck
  ];
  meta = {
    description = "Data needs to be stored more elegantly";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
  buildTools = [ cabalInstall_1_20_0_1 ];
})
