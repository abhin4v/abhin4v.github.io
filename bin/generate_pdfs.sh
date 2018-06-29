#!/bin/sh

set -x #echo on
mkdir -p pdfs/posts
python3 -m http.server &
SERVER_PID=$!
sleep 5

git status --short

CSS_CHANGED=`git status --short | grep "css/"`
if [ -z "$CSS_CHANGED" -a "$CSS_CHANGED" == "" ]; then
  git status --short | \
    grep "posts/.*" | \
    grep -v drafts | \
    grep -v "pdfs/posts" | \
    cut -c 4- | \
    sed 's/\/index.html//' | \
    sed 's/.md//' | \
    sort -u | \
    xargs -r -t -n1 -I {} ../node_modules/.bin/chrome-headless-render-pdf --chrome-option=--no-sandbox --url http://127.0.0.1:8000/{}/ --pdf pdfs/{}.pdf
else
  ls -1 -d posts/*/ | \
    sed 's/\/$//' | \
    xargs -r -t -n1 -I {} ../node_modules/.bin/chrome-headless-render-pdf --chrome-option=--no-sandbox --url http://127.0.0.1:8000/{}/ --pdf pdfs/{}.pdf
fi

kill $SERVER_PID
git add --all pdfs
