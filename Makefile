all: elm

elm: app.js styles.css

app.js: frontend/*.elm
	elm-make frontend/Main.elm --output static/app.js

styles.css: frontend/Styles/*.elm
	elm-css frontend/Styles/stylesheet.elm --output static/

clean:
	rm -r static/app.js static/styles.css node_modules/ elm-stuff/ 2> /dev/null
