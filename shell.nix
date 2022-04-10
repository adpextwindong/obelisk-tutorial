let
    pkgs = import <nixpkgs>{};
in
    pkgs.mkShell {
        name = "obelisk-tutorial";
        buildInputs = with pkgs; with haskellPackages; [
            cabal-install
            haskell.compiler.ghc8107
            SDL2
            SDL2_gfx
            pkg-config
        ];
    }
