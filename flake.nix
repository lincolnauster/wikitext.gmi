{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  outputs = { self, nixpkgs }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
  in {
    defaultPackage.x86_64-linux = pkgs.haskellPackages.developPackage {
      root = ./.;
      modifier = drv:
        pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
          [ cabal-install ghcid ]);
    };

    devShell.x86_64-linux = pkgs.mkShell {
      buildInputs = with pkgs; [ ghc cabal-install hlint ];
    };
  };
}
