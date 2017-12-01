#!/bin/sh
set -e


eval $(opam config env)
OPAMYES=1 opam pin add html_of_wiki https://github.com/$FORK_USER/html_of_wiki.git
git clone --depth 1 https://github.com/ocsigen/ocsigen.org-data.git data
cd data

how-clone
how index.wiki
how-push
