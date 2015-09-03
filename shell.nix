{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, gloss, gloss-game, lens, mtl, random
      , stdenv
      }:
      mkDerivation {
        pname = "snake";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base gloss gloss-game lens mtl random
        ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/ardumont/snake";
        description = "a simple snake game";
        license = stdenv.lib.licenses.gpl2;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
