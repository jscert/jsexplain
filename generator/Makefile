.PHONY: all clean

# requires: opam switch 4.02.1; eval `opam config env`


# OCAMLBIN=~/shared/ocamleasy/bin/
# OCAMLLIB=~/shared/ocamleasy/lib

ML_DIRS     := parsing typing utils stdlib_ml
LIB_DEP	    := str.cma
STD_DIR	    := stdlib_ml
TEST_DIR    := tests
TEST_DIR_JS := tests/js
ML_TESTS    := $(wildcard $(TEST_DIR)/*.ml)

CC          := ocamlc -c
OCAMLBUILD  := ocamlbuild -r -j 4 -classic-display \
	           $(addprefix -lflag , $(LIB_DEP) -g) \
	           $(addprefix -I ,$(ML_DIRS)) \

all: main.byte

debug: main.d.byte

%.byte: *.ml
	$(OCAMLBUILD) $@

native:
	$(OCAMLBUILD) main.native

stdlib:
	$(CC) stdlib_ml/stdlib.mli

%.inferred.mli:
	$(OCAMLBUILD) $@
	cp _build/$@ .

tests: main.byte stdlib
	$(foreach mlfile, $(ML_TESTS), ./main.byte -I tests $(mlfile);)
	mkdir -p $(TEST_DIR_JS) 
	mv $(TEST_DIR)/*.js $(TEST_DIR_JS)

clean_stdlib:
	rm -f $(STD_DIR)/*.cmi

clean_tests:
	rm -f $(TEST_DIR)/*.cmi
	rm -f $(TEST_DIR)/*.js.pre
	rm -f $(TEST_DIR_JS)/*.js

clean:
	rm -rf _build
	rm -f *.native *.byte

clean_cmi: clean_tests clean_stdlib
cleanall: clean clean_cmi
