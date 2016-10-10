PUB_FILES=driver.html libraries jquery-ui-1.11.4.custom jquery_scroll \
	  jsref/displayed_sources.js tools.js node_modules/esprima/esprima.js \
	  esprima-to-ast.js jsref/lineof.js navig-driver.js \
	  jsref/assembly.js

all: jsjsref

init:
	opam switch 4.03.0
	eval `opam config env`
	opam pin -yn add jsjsref .
	opam install -y jsjsref --deps-only

publish: generator $(PUB_FILES)
	rsync -azR --no-p --rsh=ssh -O $^ gf:/home/groups/ajacs/htdocs/jsexplain/

generator:
	$(MAKE) -C generator

jsjsref: generator
	$(MAKE) -C jsref

test_init:
	npm install

test: generator
	node_modules/.bin/mocha

.PHONY: publish jsjsref generator test_init test
