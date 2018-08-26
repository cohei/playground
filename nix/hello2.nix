with (import <nixpkgs> {});
derivation {
  name = "hello";
  builder = "${bash}/bin/bash";
  args = [ ./generic_builder.sh ];
  buildInputs = [ gnutar clang coreutils gawk gzip gnugrep gnused binutils gnumake ];
  src = ./hello-2.10.tar.gz;
  system = builtins.currentSystem;
}
