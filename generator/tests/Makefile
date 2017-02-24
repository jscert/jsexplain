test: ../monad_ppx.byte let.ml
	ocamlc -ppx ../monad_ppx.byte let.ml

%.unlog.js: %.ml ../main.byte ../monad_ppx.byte
	../main.byte -mode unlog -ppx ../monad_ppx.byte -I ../stdlib_ml $<

%.log.js: %.ml ../main.byte ../monad_ppx.byte
	../main.byte -mode log -ppx ../monad_ppx.byte -I ../stdlib_ml $<
