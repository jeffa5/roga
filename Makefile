release:
	elm make --optimize src/Main.elm

dev:
	elm-live --open -- --debug src/Main.elm
