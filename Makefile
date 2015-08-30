destroy:
	cabal sandbox delete

init:
	cabal sandbox init

install:
	nix-shell --command "cabal install"

run:
	cabal run

all: install run

spec: install
	cabal spec
