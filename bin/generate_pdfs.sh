#!/bin/sh

set -x #echo on
mkdir -p pdfs/posts
$HOME/.local/bin/site server &
ls -1 -d posts/*/ | sed 's/\/$//' | xargs -r -t -n1 -I {} ../node_modules/.bin/chrome-headless-render-pdf --url http://127.0.0.1:8000/{}/ --pdf pdfs/{}.pdf
killall site
git add --all pdfs
