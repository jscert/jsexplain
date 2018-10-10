
all: mljsref jsjsref

# Build Stages
jsjsref: generator
	cd jsref ; autoconf ; ./configure
	$(MAKE) -C jsref jsjsref

mljsref: generator # (requires the ppx)
	cd jsref ; autoconf ; ./configure
	$(MAKE) -C jsref mljsref

# Test Stages
test_init: test/data/test262

test: test_jsjsref

npm :
	npm install

test_jsjsref: jsjsref npm
	node_modules/.bin/mocha

test/data/test262: FORCE
	git submodule update --init test/data/test262

test/data/test262/%: FORCE
	git submodule update --init test/data/test262

# Documentation
doc: doc/jsref

esdocgen:
	$(MAKE) -C tools/esdocgen

doc/jsref: generator esdocgen
	$(MAKE) -C jsref doc
	rm -rf $@ || true
	mv jsref/doc_build $@

# Publication Stages
PUB_FILES=driver.html libraries jquery-ui-1.11.4.custom jquery_scroll \
	  jsref/displayed_sources.js tools.js node_modules/esprima/esprima.js \
	  esprima-to-ast.js jsref/lineof.js navig-driver.js codemirror-tabs.js \
	  jsref/assembly.js doc/documentation.html doc/screenshots doc/jsref \
	  test/data/*.js test/data/test262/harness

dist: jsjsref $(PUB_FILES)
	mkdir -p $@
	rsync -Rrv $(PUB_FILES) $@

publish: dist
	# /./ syntax tells rsync where to start relative paths from
	rsync -azR --no-p --rsh=ssh -O $^/./ gf:/home/groups/ajacs/htdocs/jsexplain/

publish-github: dist
	tools/upload-github-pages.sh dist

.PHONY: publish publish-github

# Development support
open: jsjsref
	xdg-open driver.html &

opendoc: doc
	xdg-open doc/jsref/index.html &

.merlin: FORCE
	echo "FLG -ppx $(PWD)/generator/monad_ppx.byte" > $@

.PHONY: open opendoc

# Clean stages
clean:
	$(MAKE) -C jsref clean
	$(MAKE) -C tools/esdocgen clean
	rm -Rf doc/jsref || true
	rm -Rf dist || true
	rm -Rf jsref/autom4te.cache jsref/config.status jsref/configure jsref/Makefile

FORCE:
.PHONY: jsjsref mljsref generator generator-stdlib test_init test doc esdocgen clean
.NOTPARALLEL:
