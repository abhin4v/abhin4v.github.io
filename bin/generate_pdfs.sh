#!/bin/sh

set -x #echo on
git status --short  | grep "posts/.*.html" | awk '{print $2}' | sed 's/\/index.html//' > changed-posts
$HOME/.local/bin/site server &
mkdir -p pdfs/posts
cat changed-posts | xargs -r -t -n1 -I {} chrome-headless-render-pdf --url http://127.0.0.1:8000/{}/ --pdf pdfs/{}.pdf
killall site
git add --all pdfs
