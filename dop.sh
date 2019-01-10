#!/bin/bash

# Warning: this script is not POSIX compliant because of:
# - process substitutions

set -e
[ -n "$DEBUG" ] && set -x

EXIT_SUCCESS=0
EXIT_FAILURE=1
EXIT_USAGE=2
EXIT_ENV=3
EXIT_CONFIG=4

usage() {
    echo "Usage: dop [-c CONFIG] [-v] [-h] [-r DIR] [-t <json|plain>] [-l] ROOT" >&2
    [ -z $1 ] && exit $EXIT_USAGE || exit $1
}

find_bin() {
    [ -x "$2" ] && echo $2 || {
        command -v $1 > /dev/null 2>&1 && echo $1 || {
            cat >&2 <<EOF
Could not find $1. Please check your PATH.
Alternatively you can set the environment variable $(tr 'a-z' 'A-Z' <<< $1)
with the path to the binary.
Aborting.
EOF
            exit $EXIT_ENV
        }
    }
}

check_jq() {
    command -v jq > /dev/null 2>&1 || {
        cat >&2 <<EOF
jq is required to use -t json.
Please make sure that it is installed and that your PATH is set accordingly.
jq can be found at https://stedolan.github.io/jq/ or using your system's package manager.
EOF
        exit $EXIT_ENV
    }
}

find_ohow() {
    ohow=$(find_bin ohow "$OHOW")
    wit=$(find_bin wit "$WIT")
}

process_options() {
    [ $format = plain -o $format = json ] || usage
    if [ $format = json ]; then
        check_jq
        json=true
    else
        json=false
    fi
    [ -d $wikidir ] || {
        echo "Wiki directory $wikidir does not exist. Aborting." >&2
        exit $EXIT_CONFIG
    }
    $no_run || if [ -e $root ]; then
        $force && rm -rf $root || {
            echo "Root directory $root already exists. Refusing to overwrite." >&2
            exit $EXIT_CONFIG
        }
    fi
}


first_that_exists() {
    local tst=$1
    shift
    for x in $*; do
        if [ $tst $x ]; then
            echo $x
            break
        fi
    done
}

contains_wikis() {
    [ -n "$(find $1 -maxdepth 1 -name '*.wiki')" ]
}

delete_void() {
    sed '/^\s*$/d' <&0
}

split_array() {
    tr $1 '\n' <&0 | delete_void
}
join_array() {
    local join=$(tr '\n' $1 <&0)
    echo ${join%$1}
}

infer_templates() {
    find $1 -maxdepth 1 -type f \
        -name template.wiki \
        -o -name template \
        -o -name templ.wiki \
        -o -name templ | cut -c 3- | delete_void
}
infer_config() {
    cd $wikidir
    project=$(basename `pwd`)
    default_subp=

    man=$(first_that_exists -d manual manual_wiki manual_wikis man)
    api=$(first_that_exists -d api api_wiki api_wikis apis)
    assets=$(first_that_exists -d assets files $man/assets $man/files)
    images=$(first_that_exists -d images files illustrations $man/assets $man/files $man/illustrations)
    [ -z "$images" -a -n "$assets" ] && images=$assets
    [ -z "$assets" -a -n "$images" ] && assets=$images
    contains_wikis $man || man=$(first_that_exists -d $man/src $man/wiki $man/wikis $man)

    client=$(first_that_exists -d $api/client)
    server=$(first_that_exists -d $api/server)
    [ -z $client -o -z $server ] && csw=false || csw=true

    grep -re '<<doctree' --include '*.wiki' * > /dev/null 2>&1 && menu=true || menu=false

    templates="$(infer_templates .)"
    cd - > /dev/null
}

config_error() {
    echo "An error in your configuration file has been detected. Aborting." >&2
    exit $EXIT_CONFIG
}
read_entry() {
    local val awk_sub
    if $json; then
        val=$(jq -r ".$1" $config)
        [ $? -eq 0 ] || config_error
        [ "$val" = null ] && val=
    else
        val=$(awk '$1 == "'$1'" { $1=""; gsub(/^[ \t]+/, "", $0); print $0 }' $config)
    fi
    # OKAY: checking that, an option explicitly given with a null string (plain)
    # or with null or "" (json) as value IS replaced by the default value $2.
    # The reason is that the values of manual, api, etc. are in fact
    # *relative paths* to the root directory. Hence, to specify that api=root
    # for instance, write {"api": "."} instead of {"api": ""}.
    [ -z "$val" ] && val="$2"
    echo "$val"
}
read_array() {
    local res
    local val=$(read_entry $1 $2)
    [ -z "$val" ] && return 0
    if $json; then
        res=$(jq -r '. []' <<< "$val" | delete_void)
    else
        res=$(echo -n "$val" | split_array ':')
    fi
    echo "$res"
}

read_config() {
    project=$(read_entry project $project)
    manual=$(read_entry manual $manual)
    api=$(read_entry api $api)
    assets=$(read_entry assets $assets)
    images=$(read_entry images $images)
    default_subp=$(read_entry default_subproject $default_subp)
    client=$(read_entry client $client)
    server=$(read_entry server $server)
    csw=$(read_entry csw $csw)
    menu=$(read_entry menu FLAG)
    templates=$(read_array templates '')

    # If $menu has to be inffered (no explicit value) and is false,
    # try to look for <<doctree>>s inside templates.
    [ $menu = FLAG ] && {
        cd $wikidir
        echo "$templates" | xargs grep -e '<<doctree' > /dev/null 2>&1 && menu=true || menu=false
        cd - > /dev/null
    } || true
}

check_bool() {
    [ "$1" = 'true' -o "$1" = 'false' ] || {
        echo 'The "'"$1"'" field must either be true or false.' >&2
        return 1
    }
    return 0
}
check_config() {
    check_bool "$csw" || return 1
    check_bool "$menu" || return 1
    return 0
}

show_bool() {
    $* && echo true || echo false
}
show_array() {
    [ -z "$1" ] && {
        $json && echo [] || echo ''
        return 0
    }
    if $json; then
        local quoted=$(while read -r f; do
                           echo "\"$f\""
                       done <<< "$1" | join_array ',')
        echo "[$quoted]"
    else
        echo -n $(join_array ':' <<< "$1")
    fi
}
show_config() {
    local dsp
    [ -n "$default_subp" ] && dsp="\"$default_subp\"" || dsp=null
    if $json; then
        jq . <<EOF
{"project": "$project",
"manual": "$man", "api": "$api",
"assets": "$assets", "images": "$images",
"default_subproject": $dsp,
"client": "$client", "server": "$server",
"csw": $(show_bool $csw),
"menu": $(show_bool $menu),
"templates": $(show_array "$templates")}
EOF
    else
        echo "project            $project"
        echo "manual             $man"
        echo "api                $api"
        echo "assets             $assets"
        echo "images             $images"
        echo "default_subproject $default_subp"
        echo "client             $client"
        echo "server             $server"
        echo "csw                $(show_bool $csw)"
        echo "menu               $(show_bool $menu)"
        echo "templates          $(show_array "$templates")"
    fi
}

csw_collect() {
    find $1 -name '*.wiki' -exec basename {} \; | sort
}
csw() {
    comm -12 <(csw_collect $wikidir/$client) <(csw_collect $wikidir/$server)
}

call_ohow() {
    local opts="--root $root"
    $is_local && opts="$opts --local"
    [ -n "$project" ] && opts="$opts --project $project"
    [ -n "$man" ] && opts="$opts --manual $man"
    [ -n "$api" ] && opts="$opts --api $api"
    [ -n "$assets" ] && opts="$opts --assets $assets"
    [ -n "$images" ] && opts="$opts --images $images"
    [ -n "$default_subp" ] && opts="$opts --default-subproject $default_subp"
    [ -n "$docversions" ] && opts="$opts --docversions $docversions"
    [ -n "$templates" ] && opts="$opts --template $templates"

    if $csw
    then $ohow $opts --csw <(csw) $1
    else $ohow $opts $1
    fi
}

find_wikis() {
    local exclude
    $menu && exclude="-not -name menu.wiki"
    [ -n "$templates" ] && exclude="$exclude $(while read -r t; do
                                                   echo -n "-not -path $t "
                                               done <<< "$templates")"
    find $root -name '*.wiki' $exclude | sort
}

normalize_templates() {
    templates="`while read -r t; do
                     if [ -f $root/$t ]; then echo $root/$t
                     elif [ -f $t ]; then
                         case "$t" in
                             /*) echo $t;;
                             *) echo $(pwd)/$t;;
                         esac
                     else echo Could not find template $t. Aborting.; exit $EXIT_CONFIG
                     fi
                 done <<< "$templates"`"
}

compile() {
    local t=$(mktemp)
    local wikis=$(find_wikis)
    echo $EXIT_SUCCESS >$t
    echo "$wikis" | while read -r wiki; do
        $verbose && echo -n "$wiki "
        # print failures but dop continues (-e)
        if call_ohow $wiki
        then $verbose && echo [OK]
        else
            $verbose && echo [FAIL]
            echo $EXIT_FAILURE >$t
        fi
        $keep_wikis || rm $wiki
    done
    return $(cat $t)
}


#### Proper execution
#### ================

### Find the right ohow.
find_ohow

### CLI Options
[ $# -eq 0 ] && usage

verbose=false
root="_dop"
config=
format=plain
force=false
is_local=false
keep_wikis=false
show_inferred=false
show_used=false
no_run=false
docversions=
while getopts hr:c:t:vflkiusnd: opt; do
    case "$opt" in
        r) root="$OPTARG";;
        c) config="$OPTARG";;
        t) format="$OPTARG";;
        f) force=true;;
        v) verbose=true;;
        l) is_local=true;;
        k) keep_wikis=true;;
        i) show_inferred=true;;
        u) show_used=true;;
        n) no_run=true;;
        d) docversions="$OPTARG";;
        h) usage $EXIT_SUCCESS;;
        *) usage;;
    esac
done
shift $((OPTIND-1))

[ -z $1 ] && usage
wikidir="$1"
process_options

### Configuration management
infer_config
$show_inferred && {
    echo Inferred configuration:
    show_config; echo
}
safe_read_file() {
    [ -n "$1" ] && {
        tmp=$(mktemp)
        cat $1 > $tmp
        echo $tmp
    } || echo ""
}
# Read the config once and store it in a file since it may be a process substitution.
[ -n "$config" ] && {
    config=$(safe_read_file "$config")
    read_config
} || true
check_config || {
    echo Configuration errors happened. Aborting. >&2
    exit $EXIT_CONFIG
}
$show_used && {
    echo Used configuration:
    show_config
}

$no_run && exit $EXIT_SUCCESS

### Compilation
export HOW_IN_PROJECT=t # dop.sh only exists for projects
docversions=$(safe_read_file "$docversions")
cp -r $wikidir $root
compile
if $menu && ! $keep_wikis; then
   find $root -name menu.wiki -exec rm {} \;
fi
