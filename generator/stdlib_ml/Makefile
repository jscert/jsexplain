CC := ocamlc -c -nopervasives

all: stdlib.cmi stdlib.cmo

%.cmi: stdlib.mli
	$(CC) $<

stdlib.cmo: stdlib.ml stdlib.cmi
	$(CC) $<

clean:
	rm -f *.cmi *.cmo

.PHONY: all clean
