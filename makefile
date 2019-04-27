all: game gamedraw gameobj util

game: game.ml
	ocamlbuild game.byte

gamedraw: gamedraw.ml
	ocamlbuild gamedraw.byte

gameobj: gameobj.ml
	ocamlbuild gameobj.byte

util: util.ml
	ocamlbuild util.byte

clean:
	rm -rf _build *.byte