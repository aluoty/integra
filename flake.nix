{
  description = "Integra REPL calculator — v1.0";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = false;
        };

        hspkgs = pkgs.haskellPackages;
        drv = hspkgs.callCabal2nix "integra" ./. { };

      in {
        packages.default = drv;
        packages.web = drv;

        devShells.default = pkgs.mkShell {
          inputsFrom = [ drv ];
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            zlib.dev
            pkg-config
            nodejs
            typescript
          ];

          shellHook = ''
            echo "Integra v1.0 dev shell"
            echo "  GHC:   $(ghc --version)"
            echo "  Cabal: $(cabal --version)"
          '';
        };
      });
}
