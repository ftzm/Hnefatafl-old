let pkgs = import <nixpkgs> { };
in rec {
  hnefatafl = pkgs.haskellPackages.callCabal2nix "hnefatafl" ./. { };
}
