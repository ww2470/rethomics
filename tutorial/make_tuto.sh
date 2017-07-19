#!/bin/bash
set -e 
GH_REPO="github.com/gilestrolab/rethomics.git"
FULL_REPO="https://$GH_TOKEN@$GH_REPO"

echo "Setting up..."

echo "Installing extra dependencies..."
R -e "install.packages('rmarkdown', repos='http://cran.us.r-project.org')"
R -e "install.packages('devtools', repos='http://cran.us.r-project.org')"
R -e "install.packages('DT', repos='http://cran.us.r-project.org')"
R -e "install.packages('plotly', repos='http://cran.us.r-project.org')"

echo "Working on branch $TRAVIS_BRANCH"
echo "R version: $TRAVIS_R_VERSION"

mkdir ~/rethomics_tuto
cd ~/rethomics_tuto

# setup REPO and checkout gh-pages branch
git init
git remote add origin $FULL_REPO
git fetch
git config user.name "rapporter-travis"
git config user.email "travis"
git checkout $TRAVIS_BRANCH

echo "Getting data from $RETHOMICS_TUTO_DATA ..."
echo "saving as ~/rethomics_tutorial_data.zip"
# we resolv the doi to get the last published version of the data
# todo cash data!!
wget $(curl -Ls -o /dev/null -w %{url_effective} $RETHOMICS_TUTO_DATA)/files/rethomics_tutorial_data.zip -O ~/rethomics_tutorial_data.zip --quiet
echo "Unzipping ..."
unzip ~/rethomics_tutorial_data.zip
ls
mv rethomics_tutorial_data ~



cd tutorial 
#the magic happens here!
make clean && make TUTO_DATA_DIR=~/rethomics_tutorial_data all
cd ..
cp tutorial/ ~/tutorial -r 
rm -f rethomics.pdf

# we try to push only for master and release R version!
if [ "$TRAVIS_BRANCH" == "master" ] && [ "$TRAVIS_R_VERSION" == "release" ]
#if [ "$TRAVIS_BRANCH" == "master" ]
then 
  echo 'checkout to ghpage'
  git checkout gh-pages
  mv ~/tutorial/*.html tutorial/
  #cp ~/tutorial/rethomics.pdf doc/
  
  git pull origin gh-pages
  git add tutorial
  git add doc/rethomics.pdf
  git commit -m "test autocommit after $TRAVIS_COMMIT [ci skip]"
  git status
  git push origin gh-pages        
fi
