let pkgs = import <nixpkgs> {};
in with pkgs; rec {
  snakeEnv = stdenv.mkDerivation {
    name = "env-snake";
    buildInputs = with haskellngPackages; [
      # c deps
      xorg_sys_opengl mesa_glu freeglut
      # haskell's bare deps
      cabal-install
      # not really needed
    #  cabal2nix lens hlint hdevtools zlib mtl HUnit QuickCheck hoogle
      ];
    # CABAL_INSTALL_EXTRA_FLAGS = ''
    #     --extra-lib-dirs=${xorg_sys_opengl}/lib \
    #     --extra-lib-dirs=${mesa_glu}/lib \
    #     --extra-lib-dirs=${freeglut}/lib \
    #     --extra-include-dirs=${xorg_sys_opengl}/include \
    #     --extra-include-dirs=${mesa_glu}/include \
    #     --extra-include-dirs=${freeglut}/include\
    # '';
  };
}
