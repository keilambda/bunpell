{
  description = "bunpell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , flake-parts
    , devshell
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
          _module.args.pkgs = import nixpkgs {
            localSystem = { inherit system; };
            overlays = [
              devshell.overlays.default
            ];
          };

          packages.default = bunpell;

          devShells.default = pkgs.devshell.mkShell {
            packages = [
              hpkgs.cabal-install
              hpkgs.cabal-add
              hpkgs.haskell-language-server
              hpkgs.fourmolu
              hpkgs.ghc
            ];

            commands = [
              {
                name = "coverage";
                command = ''
                  set -euo pipefail
                  cabal test --enable-coverage --test-show-details=streaming
                  cabal run bunpell --enable-coverage || true
                  outdir=coverage/html
                  rm -rf "$outdir"
                  mkdir -p "$outdir"
                  mapfile -t HPCDIRS < <(find dist-newstyle -type d -path '*/hpc/*/mix' | sort -u)
                  while IFS= read -r TIX; do
                    comp=$(basename "$TIX" .tix)
                    dest="$outdir/$comp"
                    echo "Generating HTML for $comp -> $dest"
                    ARGS=()
                    for d in "''${HPCDIRS[@]}"; do ARGS+=("--hpcdir=$d"); done
                    hpc markup "$TIX" "''${ARGS[@]}" --destdir="$dest" || true
                  done < <(find dist-newstyle -name '*.tix' | sort)
                  echo "Coverage HTML written under: $outdir"
                '';
              }
            ];
          };
        };
    };
}
