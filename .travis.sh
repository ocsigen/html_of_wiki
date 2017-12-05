#!/bin/sh
set -e

#we use the same key for cloning repositories...
echo "$SSH_KEY" |base64 -d >$HOME/.ssh/id_rsa
chmod 600 $HOME/.ssh/id_rsa
eval "$(ssh-agent -s)"
ssh-add
export SSH_KEY=


eval $(opam config env)
git clone --depth 1 https://github.com/ocsigen/ocsigen.org-data data
git clone --depth 1 https://github.com/ocsigen/$TRAVIS_REPO_SLUG
cd data
PROJECT=`basename $TRAVIS_REPO_SLUG`
git rm -rf ${PROJECT}/dev
cd ../$PROJECT
TARGET_DIR=../data/$PROJECT/dev
API_DIR=${TARGET_DIR}/api
MANUAL_SRC_DIR=${TARGET_DIR}/manual/src
MANUAL_FILES_DIR=${TARGET_DIR}/manual/files
mkdir -p ${API_DIR}
mkdir -p ${MANUAL_SRC_DIR}
mkdir -p ${MANUAL_FILES_DIR}
OPAMYES=1 . .jenkins.sh
do_build_doc

cd ../data
git commit -m `git log -n 1 --format=$PROJECT-%h`
git push 'git@github.com:ocsigen/ocsigen.org-data'
