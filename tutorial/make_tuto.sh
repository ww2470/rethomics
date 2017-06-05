#!/bin/bash
set -e 
GH_REPO="@github.com/gilestrolab/rethomics.git"
FULL_REPO="https://$GH_TOKEN$GH_REPO"


mkdir ~/rethomics_tuto
cd ~/rethomics_tuto

# setup REPO and checkout gh-pages branch
git init
git remote add origin $FULL_REPO
git fetch
git config user.name "rapporter-travis"
git config user.email "travis"
git checkout master

echo "Getting data from $RETHOMIC_TUTO_DATA ..."
wget $RETHOMICS_TUTO_DATA -O ~/rethomics_tutorial_data.zip -q
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

rm -f rethomics.pdf
# toggled out because of https://github.com/travis-ci/travis-ci/issues/7852
#make rethomics.pdf
#cp rethomics.pdf ~/tutorial/


echo 'checkout to ghpage'
git checkout gh-pages
cp ~/tutorial/*.html tutorial/
cp ~/tutorial/rethomics.pdf doc/

git add tutorial
git add doc/rethomics.pdf
git commit -m "test autocommit after $TRAVIS_COMMIT [ci skip]"
git status
git push origin gh-pages        
