{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, hashable, network, network-uri, snap, stdenv, text, transformers
, unordered-containers
}:
mkDerivation {
  pname = "snap-cors";
  version = "1.2.6";
  src = ./.;
  buildDepends = [
    attoparsec base bytestring case-insensitive hashable network
    network-uri snap text transformers unordered-containers
  ];
  homepage = "http://github.com/ocharles/snap-cors";
  description = "Add CORS headers to Snap applications";
  license = stdenv.lib.licenses.bsd3;
}
