#!/bin/sh
COMMIT=`git rev-parse HEAD`
echo Pushing on Github the documentation of each project \(on gh-pages branch\)
if [ -z "$1" ]; then
	find . -maxdepth 1 -type d -regex ".*/[^.].*$"
else
	echo ./$1
fi|
cut -c 3- |
while read line; do
        echo
	echo Processing $line...
	REPOSITORY="git@github.com:ocsigen/$line"
	if [ -d "../ocsigen.org-repositories/$line/.git" ]; then
		cd "../ocsigen.org-repositories/$line"
		git add .
		git commit -m "how-push on $COMMIT"
                echo Pushing ../ocsigen.org-repositories/$line to github
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
