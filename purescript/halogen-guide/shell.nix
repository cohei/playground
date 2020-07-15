{ pkgs ? import <nixpkgs> {} }:

let
  easyPS = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "master";
    sha256 = "014v2sk3r2x4j2g92pj70r8kyknzkycd8y04m4i66nnjr1xvr3ch";
  }) { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    easyPS.purs
    easyPS.spago
    pkgs.nodejs
  ];
}
