#!/bin/bash
[ -n $1 ] && cd $1
collect() {
    find $1 -name '*.wiki' -not -name 'menu.wiki' | awk -F/ '{ print $2 }' | sort
}
comm -12 <(collect client) <(collect server)
