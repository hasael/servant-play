let
 pkgs = import <nixpkgs>  {config.allowBroken = true;};
in
 pkgs.haskellPackages.callPackage ./default.nix { }