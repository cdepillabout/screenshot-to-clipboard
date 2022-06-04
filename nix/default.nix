let
  nixpkgs-src = builtins.fetchTarball {
    # nixos-unstable as of 2022-06-04
    url = "https://github.com/NixOS/nixpkgs/archive/236cc2971ac72acd90f0ae3a797f9f83098b17ec.tar.gz";
    sha256 = "06ydhcmzrzbvs675ycv7whhp3sjg3d76pdp27jlhx8vw9izp0ngi";
  };

  pkgs = import nixpkgs-src { overlays = [ (import ./overlay.nix) ]; };

in

pkgs
