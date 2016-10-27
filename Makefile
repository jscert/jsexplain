PUB_FILES=driver.html libraries jquery-ui-1.11.4.custom jquery_scroll \
	  jsref/displayed_sources.js tools.js node_modules/esprima/esprima.js \
	  esprima-to-ast.js jsref/lineof.js navig-driver.js \
	  jsref/assembly.js

all: generator jsjsref mljsref

init:
	opam switch 4.03.0
	eval `opam config env`
	opam pin -yn add jsjsref .
	opam pin -yn add JS_Parser "https://github.com/resource-reasoning/JS_Parser.git#v0.1.0"
	opam install -y jsjsref --deps-only

publish: generator $(PUB_FILES)
	rsync -azR --no-p --rsh=ssh -O $^ gf:/home/groups/ajacs/htdocs/jsexplain/

generator:
	$(MAKE) -C generator

generator-stdlib:
	$(MAKE) -C generator stdlib

jsjsref: generator
	$(MAKE) -C jsref jsjsref

mljsref: generator-stdlib
	$(MAKE) -C jsref mljsref

test_init:
	git submodule update --init test/test262
	npm install

test: jsjsref
	node_modules/.bin/mocha

clean:
	$(MAKE) -C generator clean
	$(MAKE) -C jsref clean

.PHONY: publish jsjsref mljsref generator generator-stdlib test_init test clean
