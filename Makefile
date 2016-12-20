all: elm

elm: app.js styles.css

app.js: frontend/*.elm
	elm-make frontend/Main.elm --output static/app.js

styles.css: frontend/styles/*.elm
	elm-css frontend/styles/stylesheet.elm --output static/

clean:
	rm -R static/app.js static/styles.css node_modules/ elm-stuff/
