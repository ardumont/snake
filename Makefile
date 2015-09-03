run:
	nix-shell --command 'cabal run'

build:
	nix-shell --command 'cabal build'

all: build run

spec:
	cabal test

sbx-init:
	cabal sandbox init

sbx-delete:
	cabal sandbox delete

to-nix:
	cabal2nix --shell . > shell.nix
	nix-shell --command 'cabal configure'
