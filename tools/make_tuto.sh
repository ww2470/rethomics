#!/bin/bash
GH_REPO="@github.com/USER/REPO.git"
FULL_REPO="https://$GH_TOKEN$GH_REPO"

mkdir out
cd out

# setup REPO and checkout gh-pages branch
git init
git remote add origin $FULL_REPO
git fetch
git config user.name "rapporter-travis"
git config user.email "travis"
#git checkout gh-pages

# do useful work for gh-pages, for example convert README.md to index.html
#pandoc ../README.md -o index.html
echo "$TRAVIS_COMMIT" > test

# commit and push changes
git add test
git commit -m "test autocommit after $TRAVIS_COMMIT"
git push origin master
