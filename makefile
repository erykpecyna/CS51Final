RESULT     = testsdl_1
SOURCES    = testsdl_1.ml
LIBS       = bigarray sdl
INCDIRS    = +sdl

all: game gamedraw gamemap gameobj util

game: game.ml
	ocamlbuild game.byte

gamedraw: gamedraw.ml
	ocamlbuild gamedraw.byte

gamemap: gamemap.ml
	ocamlbuild gamemap.byte

gameobj: gameobj.ml
	ocamlbuild gameobj.byte

util: util.ml
	ocamlbuild util.byte

clean:
	rm -rf _build *.byte