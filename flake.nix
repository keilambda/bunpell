{
  description = "bunpell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    { self
    , nixpkgs
    , flake-parts
    }@inputs: flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem = { system, pkgs, ... }:
        let
          hpkgs = pkgs.haskell.packages.ghc912;

          bunpell = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "bunpell" ./. { }) (old: {
            doCheck = true;
            doHaddock = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
          });
        in
        {
          packages.default = bunpell;

          devShells.default = pkgs.mkShell {
            buildInputs = [
              hpkgs.cabal-install
              hpkgs.cabal-add
              hpkgs.haskell-language-server
              hpkgs.fourmolu
              hpkgs.ghc
            ];
          };
        };
    };
}
