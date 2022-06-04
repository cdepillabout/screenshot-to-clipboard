let
  nixpkgs-src = builtins.fetchTarball {
    # nixos-unstable as of 2022-06-04
    url = "https://github.com/NixOS/nixpkgs/archive/236cc2971ac72acd90f0ae3a797f9f83098b17ec.tar.gz";
    sha256 = "07s5cwhskqvy82b4rld9b14ljc0013pig23i3jx3l3f957rk95pp";
  };

  pkgs = import nixpkgs-src { overlays = [ (import ./overlay.nix) ]; };

in

pkgs
