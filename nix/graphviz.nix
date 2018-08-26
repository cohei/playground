let
pkgs = import <nixpkgs> {};
mkDerivation = import ./autotools.nix pkgs;
in
mkDerivation {
  name = "graphviz";
  src = ./graphviz-2.40.1.tar.gz;
}
