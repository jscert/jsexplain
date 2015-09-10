.PHONY: all clean

# requires: opam switch 4.02.1; eval `opam config env`


# OCAMLBIN=~/shared/ocamleasy/bin/
# OCAMLLIB=~/shared/ocamleasy/lib

STD_DIR	    := stdlib_ml
TEST_DIR    := tests
TEST_DIR_JS := tests/js
ML_TESTS    := $(wildcard $(TEST_DIR)/*.ml)

CC          := ocamlc -c
OCAMLBUILD  := ocamlbuild -j 4 -classic-display -use-ocamlfind

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
	# TODO: Figure out why dependencies required to be translated first
	./main.byte -I tests tests/stack.ml
	./main.byte -I tests tests/calc.ml
	# Delete the above once figured out.
	$(foreach mlfile, $(ML_TESTS), ./main.byte -I tests $(mlfile);)
	mkdir -p $(TEST_DIR_JS) 
	mv $(TEST_DIR)/*.js $(TEST_DIR_JS)

clean_stdlib:
	rm -f $(STD_DIR)/*.cmi

clean_tests:
	rm -f $(TEST_DIR)/*.cmi
	rm -f $(TEST_DIR)/*.js.pre
	# Temp rule to remove artifacts during manual/debug creation
	rm -f $(TEST_DIR)/*.js
	rm -f $(TEST_DIR_JS)/*.js

clean:
	rm -rf _build
	rm -f *.native *.byte

clean_cmi: clean_tests clean_stdlib
cleanall: clean clean_cmi
