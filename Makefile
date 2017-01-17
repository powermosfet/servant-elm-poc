all: elm haskell

elm: app.js 

app.js: frontend/*.elm
	elm-make frontend/Main.elm --yes --output static/app.js

haskell: backend/*.hs
	stack build

run: all
	stack exec servant-elm-poc

clean:
	-rm -r static/app.js elm-stuff/ .stack-work/
