{
  # https://qcguide-hrd.appspot.com
  description = "苦しんで覚える C";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachSystem ["x86_64-darwin" "aarch64-darwin"] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        mkDerivation = name: pkgs.stdenv.mkDerivation {
          name = name;
          src = ./. + "/${name}";

          buildPhase = ''
            $CC main.c -o main
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp main $out/bin
          '';
        };

        mkPackages = names: pkgs.lib.attrsets.genAttrs names mkDerivation;

        names = [
         "chapter-1-1"
         "chapter-3-1"
         "chapter-3-2"
         "chapter-5-1"
         "chapter-6-1"
         "chapter-7-1"
         "chapter-9-1"
         "chapter-11-3"
         "chapter-15-5"
         "chapter-16-1"
         "chapter-17-1"
        ];
      in
        {
          packages = mkPackages names;
        }
    );
}
