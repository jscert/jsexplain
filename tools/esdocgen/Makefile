all: esdocgen.cmo esdocgen.cmxs

%.cmo: %.ml
	ocamlc -I +ocamldoc -c $<

%.cmxs: %.ml
	ocamlopt -I +ocamldoc -shared -o $@ $<

clean:
	rm -f *.cmo *.cmi *.cmx *.cmxs *.o

.PHONY: all clean
