#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

exec $DIR/jsref/main.byte -json \
  -test_prelude $DIR/test/data/test_prelude.js \
  -test_prelude $DIR/test/data/test262/harness/assert.js \
  -test_prelude $DIR/test/data/test262/harness/sta.js \
  -file $1
