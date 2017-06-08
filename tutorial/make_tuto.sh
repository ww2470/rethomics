#!/bin/bash
set -e 
GH_REPO="github.com/gilestrolab/rethomics.git"
FULL_REPO="https://$GH_TOKEN@$GH_REPO"

echo "Setting up..."

echo $GH_TOKEN |  sed s/0/*/g
mkdir ~/rethomics_tuto
cd ~/rethomics_tuto

# setup REPO and checkout gh-pages branch
git init
git remote add origin $FULL_REPO
git fetch
git config user.name "rapporter-travis"
git config user.email "travis"
git checkout master

echo "Getting data from $RETHOMICS_TUTO_DATA ..."
echo "saving as ~/rethomics_tutorial_data.zip"
wget $RETHOMICS_TUTO_DATA -O ~/rethomics_tutorial_data.zip
echo "Unzipping ..."
unzip ~/rethomics_tutorial_data.zip
ls
mv rethomics_tutorial_data ~
ls ~

R -e "install.packages('rmarkdown', repos='http://cran.us.r-project.org')"
R -e "install.packages('devtools', repos='http://cran.us.r-project.org')"
R -e "library(devtools); install_github('gilestrolab/rethomics',subdir='rethomics')"


cd tutorial 
#the magic happens here!
make clean && make TUTO_DATA_DIR=~/rethomics_tutorial_data all
cd ..
cp tutorial/ ~/tutorial -r 

# toggled out because of https://github.com/travis-ci/travis-ci/issues/7852
rm -f rethomics.pdf
#make rethomics.pdf
#cp rethomics.pdf ~/tutorial/


echo 'checkout to ghpage'
git checkout gh-pages
mv ~/tutorial/*.html tutorial/
#cp ~/tutorial/rethomics.pdf doc/

git add tutorial
git add doc/rethomics.pdf
git commit -m "test autocommit after $TRAVIS_COMMIT [ci skip]"
git status
git push origin gh-pages        
