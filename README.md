# JSExplain

## ** This repository is not maintained anymore. It moved here : https://gitlab.inria.fr/star-explain **

An (unofficial) ECMAScript Reference Interpreter and Double-debugger.

## Building
### Dependencies
The minimum system dependencies are:
* **OCaml** (any working version, correct version will be installed via OPAM)
* **OPAM 2.0.2**
* **Nodejs >=8** (Carbon LTS release) and the corresponding **npm**
* **Make**
* **Git**

Optional dependencies:
* rsync, for uploading builds to deployment servers
* xdg-open, for shortcut Make targets for opening built pages
* Perl / PHP interpreters, for single-use scripts within the tools directory
* Python, for the runtests distributed test execution tool in tools/runtests

### Installation and test with OPAM
```
opam switch create jsexplain 4.04.2
eval $(opam env)

opam pin -yn add JS_Parser "https://github.com/resource-reasoning/JS_Parser.git#v0.1.0"
opam install -y JS_Parser

opam repository add jsexplain https://github.com/jscert/opam-repository.git#add-pkg-fjs_of_fml

opam install -y jsexplain

firefox .opam/jsexplain/share/jsexplain/driver.html 
```

If you intend to run the test262 test suite:
```
make test_init
```

### Building
To build all default targets:
```
make
```

You can run the tool locally by opening the `driver.html` page. (`make open` is
a shortcut for this).

To test jsjsref using test262:
```sh
make test
```
### Toplevel Make Targets
* Initialisation targets
  * `test_init`: Initialises the test262 submodule and any other dependencies
    required for testing the interpreter.
  * `.merlin`: Builds the `.merlin` IDE configuration file. (Automatically built
    by `init`).
* Build Targets
  * `all` (default): Builds the `generator`, `mljsref`, `jsjsref` targets.
  * `generator`: Builds the `generator` subdirectory.
  * `jsjsref`: Builds the jsref interpreter using the ml-to-js generator.
  * `mljsref`: Builds the jsref interpreter using the standard OCaml compiler.
* Testing targets
  * `test`: Tests the generator and jsjsref.
  * `test_generator`: Runs the generator testsuite. (See generator directory for
  detail.
  * `test_jsjsref`: Tests jsjsref against the test262 test suite.
* Documentation targets
  * `doc`: Builds jsref documentation using ocamldoc with a custom 'esdocgen'
    output driver.
  * `esdocgen`: Builds the custom ocamldoc output driver.
* Distribution targets
  * `dist`: Performs a build of jsjsref and documentation and places all files
    required for distribution into the `dist` directory.
  * `publish`: Publishes the `dist` directory to the inria-gforge server (requires
    authentication).
  * `publish-github`: Publishes the `dist` directory to the `gh-pages` branch.
    This is executed on successful CI build.
* Browser shortcuts
  * `open`: Builds jsjsref and opens the `driver.html` page in your browser when
    done.
  * `opendoc`: Builds the jsref documentation and opens the documentation index in
    your browser when done.
* `clean`: Cleans all built files for the entire project, recursively.

## Testing
To select subsets of tests to run you can use the `-g` flag with the
pattern to grep for in the test name. Each test262 test case's path is
a part of the test name. For example, to select only the interpreter
run test cases for the if statement, you could use:
`npx mocha -g 'test/language/statements/if.*interprets'`

Latest version of jsjsref & debugger is automatically published at:
https://jscert.github.io/jsexplain/branch/master/driver.html

<!-- NOTE: this service is currently broken.
mljsref results are tested online and results published to:
https://psvg.doc.ic.ac.uk/ci/jscert-testing/ -->

## Architecture
The source code for the interpreter is primarily written in a subset of
OCaml with supporting runtime libraries written in both OCaml and JS.

The source code is located in the [jsref](./jsref) directory. It can either be built
using the standard OCaml compiler (to produce a result we term mljsref), or
using a custom OCaml-to-JS compiler which is located in the [generator](./generator)
directory (we term the resulting product jsjsref).

Details about the custom compilation are provided in the generator
directory.

To simplify presentation of the code, a monadic binder syntax extension is
used. This is also described in the generator directory.
