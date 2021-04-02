{ pkgs ? import <nixpkgs> {} }:
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};

  # (2)
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  nixpkgsArgs = haskellNix.nixpkgsArgs;

  # (3)
  native = import nixpkgsSrc nixpkgsArgs;

  crossArmv7l = import nixpkgsSrc (nixpkgsArgs // {
    crossSystem = native.lib.systems.examples.raspberryPi;
  });
in
  crossArmv7l.haskellPackages.callPackage ./default.nix { }