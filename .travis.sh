#!/bin/sh
set -e
ssh_key="$SSH_KEY"
export SSH_KEY=


eval $(opam config env)
git clone --depth 1 https://github.com/ocsigen/ocsigen.org-data.git data
cd data

how-clone $1
how index.wiki

echo "$ssh_key" |base64 -d >$HOME/.ssh/id_rsa
chmod 600 $HOME/.ssh/id_rsa
eval "$(ssh-agent -s)"
ssh-add
how-push $1
