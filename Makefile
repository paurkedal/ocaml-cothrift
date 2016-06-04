.PHONY: all clean test

OCAMLBUILD = OCAMLPATH=. ocamlbuild -use-ocamlfind -classic-display

all:
	pkg/pkg.ml build

clean:
	$(OCAMLBUILD) -clean

test: all
	$(OCAMLBUILD) lib/META tests/testsuite.native
	./testsuite.native
