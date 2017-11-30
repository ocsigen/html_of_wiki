#!/bin/sh
OPAM_SWITCH=${OPAM_SWITCH:-system}
FORK_USER=${FORK_USER:-ocsigen}


sudo apt-get update
sudo apt-get install -y opam

export OPAMYES=1
opam init -a --comp="$OPAM_SWITCH"
eval $(opam config env)
opam pin add html_of_wiki https://github.com/$FORK_USER/html_of_wiki.git
