#!/bin/bash

set -e
[ -n "$DEBUG" ] && set -x

usage() {
    echo "Usage: quickdop [-f] PROJECT OUTDIR [DOP_OPTIONS]" >&2
    [ -z "$1" ] && exit 1 || exit $1
}

[ $# -eq 0 ] && usage

force=false
while getopts hf opt; do
    case "$opt" in
        f) force=true;;
        h) usage 0;;
        *) usage;;
    esac
done
shift $((OPTIND-1))

[ -z "$1" ] && usage
project="$1"
outdir="$2"
shift 2

[ -e "$project" ] || {
    echo "No directory $project. Aborting." >&2
    exit 2
}
[ -e "$outdir" ] && {
    $force && rm -rf "$outdir" || {
        echo "$outdir exists. Aborting." >&2
        exit 2
    }
} || true

docversions() {
    find "$1" -maxdepth 1 -type d -exec basename {} \; | grep -E '[0-9.]+|dev' | sort
}

dv="$(docversions "$project")"
mkdir "$outdir"
for x in $project/*; do
    [ -n "$(grep "$(basename "$x")" <<< "$dv")" ] || cp -r "$x" "$outdir"
done 2>/dev/null

[ -n "$DOP" ] && dop="$DOP" || dop='dop'

ret=0
while read -r root; do
    echo "$dop -r $outdir/$root $@ -d <(echo \"$dv\") $project/$root"
    if ! $dop -r "$outdir/$root" $@ -d <(echo "$dv") "$project/$root"; then
        ret=3
    fi
done <<< "$dv"
exit $ret
