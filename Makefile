run:
	nix-shell --command 'cabal run'

build:
	nix-shell --command 'cabal build'

all: build run

spec:
	stack test

to-nix:
	cabal2nix --shell . > shell.nix
	nix-shell --command 'cabal configure'
