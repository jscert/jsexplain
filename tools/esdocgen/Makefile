all: esdocgen.cmo esdocgen.cmxs

%.cmo: %.ml
	ocamlc -I +ocamldoc -c $<

%.cmxs: %.ml
	ocamlopt -I +ocamldoc -shared -o $@ $<

clean:
	rm -f *.{cm{o,i,x,xs},o} || true

.PHONY: all clean
