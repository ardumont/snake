# specific to my nixos install -> need to remember and update on how to make it nix ok
OPTIONS=--extra-lib-dirs=/nix/store/mnhsckfydh097i747rsgxdrhlyv48vh1-glu-9.0.0/lib --extra-lib-dirs=/run/opengl-driver/lib --extra-lib-dirs=/nix/store/ffk2phimb6kgcn61jkx2hb5gm58qbdl6-freeglut-2.8.1/lib --extra-include-dirs=/nix/store/mnhsckfydh097i747rsgxdrhlyv48vh1-glu-9.0.0/include --extra-include-dirs=/run/opengl-driver/include --extra-include-dirs=/nix/store/ffk2phimb6kgcn61jkx2hb5gm58qbdl6-freeglut-2.8.1/include

destroy:
	cabal sandbox delete

init:
	cabal sandbox init

deps:
	cabal sandbox add-source deps/actionkid

install:
	cabal install $(OPTIONS)

run:
	cabal run

all: install run

spec: install
	cabal spec
