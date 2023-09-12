deps: overrides: pkgsNew: pkgsOld:

let mkHaskellOverlay =
      let extraDep = name: { "${name}" = pkgsNew.haskellPackages.callPackage (./extra-deps + "/${name}.nix") {}; };
          extraDeps = names: pkgsNew.lib.lists.foldr (a: b: a // b) {} (map extraDep names);
      in {
           haskellPackages = pkgsOld.haskellPackages.override (_: {
             overrides = final: old: overrides final old // extraDeps deps;
           });
         };

in mkHaskellOverlay
