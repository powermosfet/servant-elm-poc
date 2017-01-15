all: elm

elm: app.js 

app.js: frontend/*.elm
	elm-make frontend/Main.elm --yes --output static/app.js

clean:
	-rm -r static/app.js static/styles.css node_modules/ elm-stuff/ 
