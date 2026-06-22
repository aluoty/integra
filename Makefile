.PHONY: build run clean build-web run-web

build:
	cabal build

run:
	cabal run integra

build-web:
	cabal build integra-web

run-web:
	cabal run integra-web

clean:
	cabal clean

nix-shell:
	nix-shell -p zlib.dev
