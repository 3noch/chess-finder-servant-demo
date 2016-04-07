repo = $(shell git rev-parse --show-toplevel)

link: build
	rm -rf _dist
	mkdir _dist
	cp .stack-work/install/x86_64-osx/lts-3.6/ghcjs-0.2.0_ghc-7.10.2/bin/front.jsexe/* _dist
	cp -r static/* _dist  # to override index.html

run: link
	serve-here -d _dist

# These shenanigans are necessary because stack doesn't put GHC in the $PATH for GHCJS projects.
build:
	cd no-ghcjs \
		&& export PATH=$$(dirname $$(stack exec which ghc)):$$(dirname $$(stack exec which happy)):$$PATH \
		&& cd - \
		&& stack build

setup: setup-dev
	stack setup
	cd .. && stack setup --resolver=lts-3.6

setup-dev:
	npm install socket.io  # So GHCJSi can have browser sessions

clean:
	rm -rf _dist
	stack clean
