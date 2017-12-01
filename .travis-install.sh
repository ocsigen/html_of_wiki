#!/bin/sh
OPAM_SWITCH=${OPAM_SWITCH:-system}
FORK_USER=${FORK_USER:-ocsigen}


sudo add-apt-repository -y ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y opam

if [ -d "$HOME/.opam" ]; then
	exit 0 #do this just once
fi
OPAMYES=1 opam init -a --comp="$OPAM_SWITCH"
eval $(opam config env)
