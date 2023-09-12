{
  description = "Drop&replace reimplementation of node2nix.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
    previous.url = "github:nixos/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, previous, flake-utils, ... }:
  let mkHaskellOverlay = import ./haskell-utils.nix;

      haskellOverlay =
        mkHaskellOverlay []
          (final: _:
            {
              node2nix = final.callCabal2nix "node2nix" ./. {};
            }
          );

      forEachSystems = flake-utils.lib.eachDefaultSystem (system:
        let config = { };

            pkgs =
              import nixpkgs { inherit config system; overlays = [ haskellOverlay ]; };

            previous-pkgs = import previous { inherit system; };
            lint = previous-pkgs.callPackage ./lint.nix {};

            package = pkgs.haskellPackages.node2nix;

            node2nix = pkgs.haskell.lib.justStaticExecutables package;

            app = { type = "app"; program = "${node2nix}/bin/node2nix"; };

            devShell = package.env;

            mkCheck = name: script:
              pkgs.runCommand name {} ''
                mkdir -p $out
                ${script}
              '';

            mkChecks = pkgs.lib.attrsets.mapAttrs mkCheck;
         in {
              packages = { default = node2nix; };
              apps = { default = app; };
              devShells = {
                default = devShell;
                tools = pkgs.mkShell { buildInputs = [ pkgs.hlint ]; };
              };

              checks = mkChecks {
                "lint-nix"     = "${lint}/bin/lint ${./.}";
                "lint-haskell" = "${pkgs.hlint}/bin/hlint ${./.}";
              };

              apps.lint = {
                type = "app";
                program = "${lint}/bin/lint";
              };
            }
      );

  in forEachSystems;
}
