.PHONY: all clean

# requires: opam switch 4.02.1; eval `opam config env`


# OCAMLBIN=~/shared/ocamleasy/bin/
# OCAMLLIB=~/shared/ocamleasy/lib

ML_DIRS     := lex parsing tools typing utils stdlib_ml
LIB_DEP	    := str.cma
STD_DIR	    := stdlib_ml
TEST_DIR    := tests
TEST_DIR_JS := tests/js
ML_TESTS    := $(wildcard $(TEST_DIR)/*.ml)

CC          := ocamlc -c
OCAMLBUILD  := ocamlbuild -r -j 4 -classic-display \
	           $(addprefix -lflag , $(LIB_DEP)) \
	           $(addprefix -I ,$(ML_DIRS)) \

all: main.byte

main.byte:
	$(OCAMLBUILD) main.byte

native:
	$(OCAMLBUILD) main.native

stdlib:
	$(CC) stdlib_ml/stdlib.mli

tests: main.byte stdlib
	$(foreach mlfile, $(ML_TESTS), ./main.byte $(mlfile);)
	mv $(TEST_DIR)/*.js $(TEST_DIR_JS)

clean_stdlib:
	rm -f $(STD_DIR)/*.cmi

clean_tests:
	rm -f $(TEST_DIR)/*.cmi
	rm -f $(TEST_DIR_JS)/*.js

clean:
	rm -rf _build
	rm -f *.native *.byte

cleanall: clean clean_tests clean_stdlib
