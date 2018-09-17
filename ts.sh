#!/bin/sh

blu=$(tput setaf 4)
red=$(tput setaf 1)
grn=$(tput setaf 2)
yel=$(tput setaf 3)
bld=$(tput bold)
std=$(tput sgr0)

ENABLE_DEBUG=''
PREFIX=$bld

dbg() {
   [ -z $ENABLE_DEBUG ] && echo $PREFIX$blu$*$std >&1
}

msg() {
    echo $PREFIX$grn$*$std >&2
}

wrn() {
    echo $PREFIX$yel$*$std >&1
}

err() {
    echo $PREFIX$red$*$std >&2
}


[ -n "$1" -a -d "$1" ] && REALDIR="$1" || REALDIR=doc
TSDIR=_ts_dir
TSF=$(mktemp)
cp -r $REALDIR $TSDIR

test -x ohow.byte || make ohow || {
        err "could not find nor build 'ohow.byte' using 'make ohow'"
        exit 1
    }

MSG='no crash'
find $TSDIR -name '*.wiki' | while read -r wiki; do
    ./ohow.byte -p $wiki > /dev/null || {
        err "$MSG -- FAIL for file $wiki"
    }
done

MSG='--print and --output consistency'
find $TSDIR -name '*.wiki' | while read -r wiki; do
    ./ohow.byte -o $TSF $wiki && ./ohow.byte -p $wiki | cmp $TSF || {
            err "$MSG -- FAIL for file $wiki"
        }
done

MSG='each wiki gives an html with the same name'
find $TSDIR -name '*.wiki' -exec ./ohow.byte {} \;
find $TSDIR -name '*.wiki' | while read -r wiki; do
    test -f "${wiki/%wiki/html}" || {
        err "$MSG -- FAIL for file $wiki"
    }
done

msg test suite end

rm -rf $TSF
rm -rf $TSDIR
