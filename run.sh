#!/bin/sh

cargo run && \
mv out.js out.js.old && \
js-beautify out.js.old > out.js && \
rm out.js.old && \
node out.js || \
echo "Compilation error"
