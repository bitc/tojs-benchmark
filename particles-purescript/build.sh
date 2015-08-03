#!/bin/sh

# This script assumes that Build.hs is the only build script (it should not
# import any other local files)

set -e

if [ ! _build/build -nt Build.hs ]; then
	mkdir -p _build || exit 1
	ghc --make Build.hs -rtsopts -with-rtsopts=-I0 -outputdir=_build -o _build/build || exit 1
fi

_build/build "$@"
