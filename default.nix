{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
with import <nixpkgs> {};
let
  inherit (haskellPackages) cabal cabalInstall_1_18_0_2 snap;
in cabal.mkDerivation (self: {
  pname = "snap-cors";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ snap ];
  buildTools = [ cabalInstall_1_18_0_2 ];
})
