{
  inputs = {
    flake-utils.url = "github:Numtide/flake-utils";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com" # Seems to be more reliable than the iog cache.
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
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
          inherit system;
          inherit (haskell-nix) config;
          overlays = [
            haskell-nix.overlay
          ];
        };

        nbparts-project = pkgs.haskell-nix.project' {
          src = ./.;

          compiler-nix-name = "ghc984";

          shell = {
            tools = {
              cabal = { };
              haskell-language-server = { };
            };

            buildInputs = [
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

        flake = nbparts-project.flake {
          crossPlatforms = ps: [
            ps.musl64
            ps.ucrt64
            ps.x86_64-darwin
          ];
        };

        mkPkgsApps =
          out-name:
          let
            out = flake.${out-name};
            nbparts = out."nbparts:exe:nbparts";
          in
          out
          // {
            default = nbparts;
            inherit nbparts;
            nbparts-static = out."x86_64-unknown-linux-musl:nbparts:exe:nbparts";
          };
      in
      flake
      // {
        packages = mkPkgsApps "packages";
        apps = mkPkgsApps "apps";
        devShells = flake.devShells // {
          ci = pkgs.mkShell {
            packages = [
              pkgs.upx
            ];
          };
        };
      }
    );
}
