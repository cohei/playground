{ pkgs ? import <nixpkgs> {} }:

let
  easyPS = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "master";
    sha256 = "12hk2zbjkrq2i5fs6xb3x254lnhm9fzkcxph0a7ngxyzfykvf4hi";
  }) { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = [
    easyPS.purs
    easyPS.spago
    pkgs.nodejs
  ];
}
