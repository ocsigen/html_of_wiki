#!/bin/sh
set -e
ssh_key="$SSH_KEY"
export SSH_KEY=


eval $(opam config env)
git clone --depth 1 https://github.com/ocsigen/ocsigen.org-data.git data
cd data

how-clone $1
how index.wiki

eval "$(ssh-agent -s)"
echo before
ls -l $HOME/.ssh
echo "$ssh_key" >$HOME/.ssh/id_rsa
chmod 600 $HOME/.ssh/id_rsa
echo after
ls -l $HOME/.ssh
ssh-add
how-push $1
