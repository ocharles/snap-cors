{ cabal, networkUri, snap }:
cabal.mkDerivation (self: {
  pname = "snap-cors";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ networkUri snap ];
})
