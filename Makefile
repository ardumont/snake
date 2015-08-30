destroy:
	cabal sandbox delete

init:
	cabal sandbox init

deps:
	cabal sandbox add-source deps-src/actionkid

install:
	nix-shell --command 'cabal install'

run:
	cabal run

all: install run

spec: install
	cabal spec
