with (import <nixpkgs-unstable> {});
mkShell {
  buildInputs = [
    haskell.compiler.ghc941
  ];
}
