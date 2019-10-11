#!/bin/bash -e

stack build --fast --ghc-options=-j6
rm -rf output/Example output/OtherModule
stack exec -- purs compile 'tests/support/bower_components/purescript-*/src/**/*.purs' src/*.purs
#cat output/OtherModule/index.js
#cat output/Example/index.js
# node -e 'require("./output/Example").main();'
