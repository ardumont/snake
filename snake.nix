{ mkDerivation, ActionKid, base, gloss, lens, mtl, random, stdenv
}:
mkDerivation {
  pname = "snake";
  version = "0.1.0.0";
  sha256 = "dummy";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ActionKid base gloss lens mtl random
  ];
  homepage = "https://github.com/ardumont/snake";
  description = "a simple snake game";
  license = stdenv.lib.licenses.gpl2;
}
