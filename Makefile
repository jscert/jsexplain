PUB_FILES=driver.html libraries jquery-ui-1.11.4.custom jquery_scroll \
	generator/tests/jsref/displayed_sources.js tools.js node_modules/esprima/esprima.js \
	esprima-to-ast.js generator/tests/jsref/lineof.js navig-driver.js \
	generator/tests/jsref/assembly.js

publish: $(PUB_FILES)
	rsync -azR --no-p --rsh=ssh -O $^ gf:/home/groups/ajacs/htdocs/jsexplain/

.PHONY: publish
