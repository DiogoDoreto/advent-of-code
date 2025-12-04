{
  description = "My Advent of Code 2025";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        sbcl' = pkgs.sbcl.withPackages (
          ps: with ps; [
            qlot
            qlot-cli
          ]
        );
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            sbcl'
          ];
        };
      }
    );
}
