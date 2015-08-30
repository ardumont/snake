run:
	stack exec snake-exe

build:
	stack build

all: build run

spec:
	stack test
