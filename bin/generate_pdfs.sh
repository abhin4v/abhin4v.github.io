#!/bin/sh

set -x #echo on
mkdir -p pdfs/posts
python3 -m http.server &
SERVER_PID=$!
sleep 5
git status --short | grep "posts/.*.html" | cut -c 4- | sed 's/\/index.html//' | xargs -r -t -n1 -I {} ../node_modules/.bin/chrome-headless-render-pdf --chrome-option=--no-sandbox --url http://127.0.0.1:8000/{}/ --pdf pdfs/{}.pdf
kill $SERVER_PID
git add --all pdfs
