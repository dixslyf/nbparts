{
  inputs = {
    flake-utils.url = "github:Numtide/flake-utils";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      haskell-nix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit
            system
            overlays
            ;
          inherit (haskell-nix) config;
        };

        overlays = [
          haskell-nix.overlay
          (final: _prev: {
            nbparts = final.haskell-nix.project' {
              src = ./.;

              compiler-nix-name = "ghc984";

              shell = {
                tools = {
                  cabal = { };
                  haskell-language-server = { };
                };

                buildInputs = with pkgs; [
                  # https://github.com/input-output-hk/haskell.nix/issues/1776
                  (pkgs.writeScriptBin "haskell-language-server-wrapper" ''
                    #!${pkgs.stdenv.shell}
                    exec haskell-language-server "$@"
                  '')

                  pkgs.ormolu

                  (pkgs.python3.withPackages (
                    ps: with ps; [
                      jupyterlab
                      matplotlib
                      pillow
                      ipympl
                    ]
                  ))

                  pkgs.nodePackages.json-diff
                ];
              };
            };
          })
        ];

        flake = pkgs.nbparts.flake {
          crossPlatforms = ps: [
            ps.ucrt64
            ps.x86_64-darwin
          ];
        };
      in
      flake
      // {
        packages = flake.packages // {
          default = flake.packages."nbparts:exe:nbparts";
        };
      }
    );
}
