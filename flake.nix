{
  description = "Integra REPL calculator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            zlib.dev
          ];

          shellHook = ''
            echo "Integra dev shell loaded"
            echo "  GHC:    $(ghc --version)"
            echo "  Cabal:  $(cabal --version)"
          '';
        };
      });
}
