parse.byte: syntaxConv.ml parse.ml
	ocamlfind ocamlc -package js_of_ocaml.ppx -package js_of_ocaml -linkpkg -o parse.byte syntax.ml syntaxConv.ml parse.ml

%.unlog.js: %.ml ../generator/main.byte ../generator/monad_ppx.byte
	../generator/main.byte -mode unlog -ppx ../generator/monad_ppx.byte -I ../generator/stdlib_ml $<

parse.js: syntax.unlog.js parse.byte
	js_of_ocaml parse.byte
