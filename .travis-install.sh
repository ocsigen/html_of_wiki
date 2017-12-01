#!/bin/sh
OPAM_SWITCH=${OPAM_SWITCH:-system}
FORK_USER=${FORK_USER:-ocsigen}


sudo add-apt-repository -y ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y opam

export OPAMYES=1
if [ -d "$HOME/.opam" ]; then
	opam remove html_of_wiki
	exit 0 #do this just once
fi
opam init -a --comp="$OPAM_SWITCH"
eval $(opam config env)
opam pin add html_of_wiki https://github.com/$FORK_USER/html_of_wiki.git
