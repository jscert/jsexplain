#!/bin/sh

cat stdlib_js/stdlib.debug.js >> _____tmp.js
cat $1 >> _____tmp.js

node  _____tmp.js

rm  _____tmp.js
