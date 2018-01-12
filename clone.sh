#!/bin/sh
if [ -z "$1" ]; then
	find . -maxdepth 1 -type d -regex ".*/[^.].*$"
else
	echo ./$1
fi|
cut -c 3- |
while read line; do
	REPOSITORY="https://github.com/ocsigen/$line"
	echo Processing $line...
	mkdir ../gen 2>/dev/null
	cd ../gen
	if [ -d "$line" ]; then
		cd "$line"
		if [ -d .git ]; then
			git pull origin gh-pages
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
	elif git clone -b gh-pages --depth 1 $REPOSITORY; then
		true #nothing else to do!
	else
		mkdir ../gen/"$line"
		cd ../gen/"$line"
		git init
		git checkout -b gh-pages
		git remote add origin $REPOSITORY
		cd - >/dev/null
	fi
done
