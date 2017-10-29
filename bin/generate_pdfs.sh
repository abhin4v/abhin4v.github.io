#!/bin/sh

set -x #echo on
mkdir -p pdfs/posts
python -m SimpleHTTPServer &
SERVER_PID=$!
sleep 5
ls -1 -d posts/*/ | sed 's/\/$//' | xargs -r -t -n1 -I {} ../node_modules/.bin/chrome-headless-render-pdf --url http://127.0.0.1:8000/{}/ --pdf pdfs/{}.pdf
kill $SERVER_PID
git add --all pdfs
