all: generator mljsref jsjsref

# Init stages
init:
	opam switch 4.04.0
	eval `opam config env`
	opam pin -yn add jsjsref .
	opam pin -yn add JS_Parser "https://github.com/resource-reasoning/JS_Parser.git#v0.1.0"
	opam install -y jsjsref --deps-only
	@echo
	@echo 'You now need to execute: eval `opam config env`'

# Build Stages
generator:
	$(MAKE) -C generator

jsjsref: generator
	$(MAKE) -C jsref jsjsref

mljsref: generator # (requires the ppx)
	$(MAKE) -C jsref mljsref

# Test Stages
test_init:
	git submodule update --init test/data/test262
	npm install

test: jsjsref
	node_modules/.bin/mocha

# Documentation
doc: doc/jsref

doc/jsref:
	$(MAKE) -C jsref doc
	mv jsref/doc_build doc/jsref

# Publication Stages
PUB_FILES=driver.html libraries jquery-ui-1.11.4.custom jquery_scroll \
	  jsref/displayed_sources.js tools.js node_modules/esprima/esprima.js \
	  esprima-to-ast.js jsref/lineof.js navig-driver.js \
	  jsref/assembly.js doc/documentation.org doc/screenshots doc/jsref

dist: jsjsref $(PUB_FILES)
	mkdir -p $@
	rsync -Rrv $(PUB_FILES) $@

publish: dist
	# /./ syntax tells rsync where to start relative paths from
	rsync -azR --no-p --rsh=ssh -O $^/./ gf:/home/groups/ajacs/htdocs/jsexplain/

publish-github: dist
	tools/upload-github-pages.sh dist

# Clean stages
clean:
	$(MAKE) -C generator clean
	$(MAKE) -C jsref clean
	rm -Rf doc/jsref || true
	rm -Rf dist || true

.PHONY: jsjsref mljsref generator generator-stdlib test_init test doc publish publish-github clean
