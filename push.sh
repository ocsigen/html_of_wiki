#!/bin/sh
COMMIT=`git rev-parse HEAD`
echo Pushing on Github the documentation of each project \(on gh-pages branch\)
echo
echo Firstly, I move data from ../ocsigen.org-repositories/ocsigen.github.io/1.0/manual to the root of the project
mv -f ../ocsigen.org-repositories/ocsigen.github.io/1.0/manual/* ../ocsigen.org-repositories/ocsigen.github.io/
echo renaming intro.html to index.html
mv -f ../ocsigen.org-repositories/ocsigen.github.io/intro.html ../ocsigen.org-repositories/ocsigen.github.io/index.html
echo Installing ../html_of_wiki/client.js in ../ocsigen.org-repositories/ocsigen.github.io/css/
cp -f ../html_of_wiki/client.js ../ocsigen.org-repositories/ocsigen.github.io/js/
echo Please commit stylesheet directly in ocsigen.github.io repository
echo
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
        if [ "$line" = "ocsigen.github.io" ]; then
            BRANCH=master
        else
	    BRANCH=gh-pages
        fi
	if [ -d "../ocsigen.org-repositories/$line/.git" ]; then
		cd "../ocsigen.org-repositories/$line"
		git add .
		git commit -m "how-push on $COMMIT"
                echo Pushing ../ocsigen.org-repositories/$line to github
                echo git push "$REPOSITORY" $BRANCH
		if ! git push "$REPOSITORY" $BRANCH; then
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
