with import <nixpkgs> {};

let
  env = haskellPackages.ghcWithPackages
    (p: [ (p.callPackage ./satanais.nix {}) p.megaparsec p.parser-combinators ]);
in
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [ env ];
  shellHook = ''
  ghci && exit
  '';
}
