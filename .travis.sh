#!/bin/sh
set -e


eval $(opam config env)
git clone --depth 1 https://github.com/ocsigen/ocsigen.org-data.git data
cd data

how-clone $1
how index.wiki
how-push $1
