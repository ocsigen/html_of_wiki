#!/bin/sh
set -e

#we use the same key for cloning repositories...
echo "$SSH_KEY" |base64 -d >$HOME/.ssh/id_rsa
chmod 600 $HOME/.ssh/id_rsa
eval "$(ssh-agent -s)"
ssh-add
export SSH_KEY=


eval $(opam config env)
git clone --depth 1 https://github.com/ocsigen/ocsigen.org-data
git clone --depth 1 https://github.com/ocsigen/tyxml
cd tyxml
OPAMYES=1 sh .jenkins.sh

echo TODO commit
