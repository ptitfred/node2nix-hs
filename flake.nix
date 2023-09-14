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
              node2nix-hs = final.callCabal2nix "node2nix-hs" ./. {};
            }
          );

      forEachSystems = flake-utils.lib.eachDefaultSystem (system:
        let pkgs =
              import nixpkgs { inherit system; overlays = [ haskellOverlay ]; };

            previous-pkgs = import previous { inherit system; };
            lint = previous-pkgs.callPackage ./lint.nix {};

            package = pkgs.haskellPackages.node2nix-hs;

            node2nix-hs = pkgs.haskell.lib.justStaticExecutables package;

            app = { type = "app"; program = "${node2nix-hs}/bin/node2nix-hs"; };

            devShell = package.env;

            mkCheck = name: script:
              pkgs.runCommand name {} ''
                mkdir -p $out
                ${script}
              '';

            mkChecks = pkgs.lib.attrsets.mapAttrs mkCheck;
         in {
              packages = { inherit node2nix-hs; default = node2nix-hs; };
              apps = { default = app; };
              devShells = {
                default = devShell;
                tools = pkgs.mkShell { buildInputs = [ pkgs.hlint ]; };
              };

              checks = mkChecks {
                "lint-nix"     = "${lint}/bin/lint ${./.}";
                "lint-haskell" = "${pkgs.hlint}/bin/hlint --ignore-glob=${./.}/src/NodeEnv.hs ${./.}";
              };

              apps.lint = {
                type = "app";
                program = "${lint}/bin/lint";
              };
            }
      );

      overlay = _: prev: { inherit (forEachSystems.packages.${prev.system}) node2nix-hs; };

  in forEachSystems // { overlays.default = overlay; };
}
