with import <nixpkgs> {};

derivation {
  name = "simple";
  builder = "${bash}/bin/bash";
  args = [ ./builder.sh ];
  inherit clang coreutils;
  src = ./simple.c;
  system = builtins.currentSystem;
}
