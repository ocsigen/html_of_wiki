#!/bin/sh
OPAM_SWITCH=${OPAM_SWITCH:-system}
FORK_USER=${FORK_USER:-ocsigen}


sudo add-apt-repository -y ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y opam

export OPAMYES=1
if ! [ -f "$HOME/.opam/config" ]; then
	opam init -a --comp="$OPAM_SWITCH"
fi
eval $(opam config env)
opam pin add -n html_of_wiki https://github.com/$FORK_USER/html_of_wiki.git
opam reinstall html_of_wiki
