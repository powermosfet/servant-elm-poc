all: elm

elm: app.js styles.css

app.js: frontend/*.elm
	elm-make frontend/Main.elm --output static/app.js

styles.css: frontend/styles/*.elm
	elm-css frontend/styles/stylesheet.elm --output static/styles.css


