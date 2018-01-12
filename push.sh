#!/bin/sh
COMMIT=`git rev-parse HEAD`
if [ -z "$1" ]; then
	find . -maxdepth 1 -type d -regex ".*/[^.].*$"
else
	echo ./$1
fi|
cut -c 3- |
while read line; do
	echo Processing $line...
	REPOSITORY="git@github.com:ocsigen/$line"
	if [ -d "../gen/$line/.git" ]; then
		cd "../gen/$line"
		git add .
		git commit -m "push.sh on $COMMIT"
		if ! git push "$REPOSITORY" gh-pages; then
			if [ ! -t 0 ]; then
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
