{
  description = "dev env for 'Write yourself a scheme in 48 hours'";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      ghc = pkgs.haskellPackages.ghcWithPackages (hp: [
        hp.parsec
        hp.mtl_2_3_2
      ]);
    in {
      formatter = pkgs.alejandra;
      devShells.default = pkgs.mkShell {
        packages = [
          ghc
          pkgs.cabal-install
          pkgs.haskellPackages.cabal-install
          pkgs.haskell-language-server
        ];
      };
    });
}
