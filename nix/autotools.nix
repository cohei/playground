pkgs: attrs:
with pkgs;
let defaultAttrs = {
  builder = "${bash}/bin/bash";
  args = [ ./builder.sh ];
  baseInputs = [ gnutar gcc coreutils gawk gzip gnugrep gnused binutils gnumake clang findutils patchelf ];
  buildInputs = [];
  system = builtins.currentSystem;
};
in derivation (defaultAttrs // attrs)
