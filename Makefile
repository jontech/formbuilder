.PHONY: build repl
all: build

build:
	node_modules/elm/bin/elm make src/Main.elm --output main.js

repl:
	node_modules/elm/bin/elm repl
