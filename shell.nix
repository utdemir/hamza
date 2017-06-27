{ pkgs ? import ../github/nixos/nixpkgs {} }:

(import ./. { inherit pkgs; }).env
