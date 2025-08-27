{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:Numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) lib;

        ghc = pkgs.haskell.compiler.ghc984;
        hls = pkgs.haskell-language-server;
      in
      {
        devShells = {
          default = pkgs.mkShell (
            let
              packages = [
                ghc
                pkgs.cabal-install
                hls
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
            in
            {
              inherit packages;
              LD_LIBRARY_PATH = lib.makeLibraryPath [
                pkgs.pkg-config
                pkgs.zlib
              ];
            }
          );
        };
      }
    );
}
