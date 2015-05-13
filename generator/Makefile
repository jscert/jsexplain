.PHONY: all clean

ML_DIRS    := lex parsing tools typing utils
OCAMLBUILD := ocamlbuild -j 4 -classic-display \
	           $(addprefix -I ,$(ML_DIRS)) \

all:
	$(OCAMLBUILD) main.byte


native:
	$(OCAMLBUILD) main.native

test:
	./main.byte _test.ml

clean:
	rm -rf _build
	rm -f *.native *.byte

#	rm -f *~
