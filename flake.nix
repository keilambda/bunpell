{
  description = "bunpell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { localSystem = { inherit system; }; };
      hpkgs = pkgs.haskell.packages."ghc912";

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
        ];
      };
    });
}
