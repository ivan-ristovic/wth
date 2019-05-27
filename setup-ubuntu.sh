#!/bin/bash

function inst {
	if ! apt-get -qq install "$@" > /dev/null; then
		echo ">>> ERROR WHILE INSTALLING PACKAGE(S):"
		echo "$@"
	fi
}

if [ "$EUID" -ne 0 ]
	then echo ">> Please run as sudo."
	exit 1
fi

deps="dependencies.txt"
while IFS= read -r pkg
do
	echo "Installing $pkg"
	inst "$pkg"
done < "$deps"

echo "All done! Have a nice day :)"