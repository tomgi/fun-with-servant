install:
	stack install

install-all:
	$(MAKE) install
	$(MAKE) elm-export
	$(MAKE) elm-compile

elm-export:
	stack install --exec elm-export

elm-compile:
	elm make src/frontend/Main.elm --output public/index.js

live-back:
	ghcid --test=:main

live-front:
	xdg-open "http://localhost:8080/live.html"
	elm-live src/frontend/Main.elm -- --output public/index.js

run:
	$(MAKE) install-all
	stack exec fun-with-servant
