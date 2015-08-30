let pkgs = import <nixpkgs> {};
in with pkgs; rec {
  snakeEnv = stdenv.mkDerivation {
    name = "env-snake";
    buildInputs = with haskellngPackages; [
      # c deps
      xorg_sys_opengl mesa_glu freeglut
      # haskell's bare deps
      cabal-install
    ];
    LD_LIBRARY_PATH = "${xorg_sys_opengl}/lib:${mesa_glu}/lib:${freeglut}/lib:/run/opengl-driver/lib:/run/opengl-driver-32/lib";
  };
}
