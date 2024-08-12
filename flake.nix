{
  description = "YTMusicRipper";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          # inherit system overlays;
        };
      in
        with pkgs;
        {
          devShells.default = mkShell {
            buildInputs = [
              pkg-config
              ghc
              # haskell.compiler.ghc88
              # haskell.compiler.ghc94
              stack
              pkg-config
              zlib
              # chromedriver
              geckodriver
              selenium-server-standalone
              haskell-language-server
              # google-chrome
              # chrome
              # haskellPackages.selenium-server
# haskellPackages.selenium
            ];
            shellHook="
            ";
          };
        }
    );
}
