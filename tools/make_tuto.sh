#!/bin/bash
GH_REPO="@github.com/gilestrolab/rethomics.git"
FULL_REPO="https://$GH_TOKEN$GH_REPO"

cd

mkdir out
cd out

# setup REPO and checkout gh-pages branch
git init
git remote add origin $FULL_REPO
git fetch
git config user.name "rapporter-travis"
git config user.email "travis"
git checkout gh-page

echo "Getting data from $RETHOMIC_TUTO_DATA"
wget $RETHOMIC_TUTO_DATA -O ~/rethomic_tutorial_data.zip
unzip ~/rethomic_tutorial_data.zip
ls ~
tree ~/rethomic_tutorial_data/

# do useful work for gh-pages, for example convert README.md to index.html
#pandoc ../README.md -o index.html
echo 'making file `test`'
echo "$TRAVIS_COMMIT" > test
cat test

# commit and push changes
git add test
git commit -m "test autocommit after $TRAVIS_COMMIT [ci skip]"
echo "pushing to master"
git push origin master
echo "DONE"
