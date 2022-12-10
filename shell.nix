{ system ? builtins.currentSystem }:

with import <nixpkgs> { inherit system; };

mkShell {
  buildInputs = [
    haskell.compiler.ghc942
    cabal-install
    haskell-ci
  ];
}
