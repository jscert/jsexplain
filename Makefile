
all:

# Copy across a new version of the interpreter
# WARNING: Loses ALL local changes.
fresh_interp:
	$(MAKE) -C .. extract_interpreter
	cp ../interp/src/*.ml{,i} ../interp/src/extract/*.ml{,i} src
	./convert-ml-strings.pl -i.bak src/*
