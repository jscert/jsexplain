.PHONY: all clean

# requires: opam switch 4.02.1; eval `opam config env`


# OCAMLBIN=~/shared/ocamleasy/bin/
# OCAMLLIB=~/shared/ocamleasy/lib

ML_DIRS    := lex parsing tools typing utils
OCAMLBUILD := ocamlbuild -r -j 4 -classic-display \
	           $(addprefix -I ,$(ML_DIRS)) \

all: main.byte

main.byte:
	$(OCAMLBUILD) main.byte

native:
	$(OCAMLBUILD) main.native

test: main.byte
	./main.byte _test.ml

interp: main.byte
	./main.byte _interp.ml

clean:
	rm -rf _build
	rm -f *.native *.byte

#	rm -f *~
