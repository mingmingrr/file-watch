{ callCabal2nix ? (import <nixpkgs> {}).haskellPackages.callCabal2nix }:
callCabal2nix "file-watch" ./. {}
