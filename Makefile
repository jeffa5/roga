release:
	elm make --optimize src/Main.elm --output=main.js

dev:
	elm-live --open -- --debug src/Main.elm --output=main.js
