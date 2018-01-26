#!/bin/sh
echo Cloning branch gh-pages of all projects in ../ocsigen.org-repositories
if [ -z "$1" ]; then
	find . -maxdepth 1 -type d -regex ".*/[^.].*$"
else
	echo ./$1
fi|
cut -c 3- |
while read line; do
    echo
    echo Processing $line...
    mkdir ../ocsigen.org-repositories 2>/dev/null
    cd ../ocsigen.org-repositories
    REPOSITORY="https://github.com/ocsigen/$line"
    if [ "$line" = "ocsigen.github.io" ]; then
        BRANCH=master
    else
        BRANCH=gh-pages
    fi
    if [ -d $line ]; then
		cd "$line"
		if [ -d .git ]; then
			git pull origin $BRANCH
			if [ `git diff --name-only --diff-filter=U |wc -l` -ne 0 ]; then
				echo Please resolve conflicts in $line and press Ctrl-D...
				$SHELL
			fi
		else
			echo `pwd` is not a Git repository. 2>&1
			echo Exiting due to hygiene violations. 2>&1
			exit 1
		fi
		cd - >/dev/null
	elif git clone -b $BRANCH --depth 1 $REPOSITORY $line; then
		true #nothing else to do!
	else
		mkdir ../ocsigen.org-repositories/"$line"
		cd ../ocsigen.org-repositories/"$line"
		git init
		git checkout -b $BRANCH
		git remote add origin $REPOSITORY
		cd - >/dev/null
	fi
done
