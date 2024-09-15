{
  description = "Dev shell for pomodoro-clock-cli development";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        package = haskellPackages.developPackage {
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with haskellPackages; [
              cabal-install
              ghcid
              haskell-language-server
            ]);
        };
      in {
        packages.default = package;
        devShells.default = package.env;
      });
}
