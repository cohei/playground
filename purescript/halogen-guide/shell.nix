{ pkgs ? import <nixpkgs> {} }:

let
  easyPS = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "master";
    sha256 = "0gnwymgm4i5y9vknpcsr99pwy76w14nclqxb6xmmzlw2s8fx85hm";
  }) { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    easyPS.purs
    easyPS.spago
    pkgs.nodejs
  ];
}
