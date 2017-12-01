#!/bin/sh
if [ `git st --porcelain |wc -l` -ne 0 ]; then
	#no need to test interactivity, since we've (normally) just cloned it
	echo Please commit your changes to ensure this is reproducible. 2>&1
	exit 1
fi
COMMIT=`git rev-parse HEAD`
if [ -z "$1" ]; then
	find . -type d -depth 1 -regex ".*/[^.].*$"
else
	echo ./$1
fi|
cut -c 3- |
while read line; do
	echo Processing $line...
	if [ -d "../gen/$line/.git" ]; then
		cd "../gen/$line/.git"
		git add .
		git commit -m "push.sh on $COMMIT"
		if ! git push origin gh-pages; then
			if [ -z "$PS1" ]; then
				echo There are conflicts, aborting!
				exit 1
			else
				echo Please resolve conflicts and press Ctrl-D...
				$SHELL
			fi
		fi
		cd -
	else
		echo Not a Git repository. 2>&1
		exit 1
	fi
done
