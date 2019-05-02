all: game gamedraw gamemap gameobj util gameState

game: game.ml
	ocamlbuild game.byte

gamedraw: gamedraw.ml
	ocamlbuild gamedraw.byte

gamemap: gamemap.ml
	ocamlbuild gamemap.byte

gameobj: gameobj.ml
	ocamlbuild gameobj.byte

gameState: gameState.ml
	ocamlbuild gameState.byte

util: util.ml
	ocamlbuild util.byte

clean:
	rm -rf _build *.byte