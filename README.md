# G.O.A.T. ![Travis](https://travis-ci.org/thebritican/goat.svg?branch=master)
### Graphics Ornamentation and Annotation Tool
### a.k.a. Greatest Of All Time

![demo](https://cloud.githubusercontent.com/assets/3099999/24594663/4055c25e-17e4-11e7-8844-a63a395db5f1.gif)


Minimally annotate images with common features like:

- Arrows
- Lines
- Rectangles
- Ellipses
- Text Boxes

## Motivation

This will soon be a [Zendesk Editor App](https://www.zendesk.com/apps/directory/#Compose_&_Edit)

## Credits

👏 Huge thanks to [Jian Wei Liau](https://twitter.com/madebyjw) for the beautiful icons and logo! 👏

🐐 Thanks to [Alan Hogan](https://github.com/alanhogan) for the acronym behind the 🐐

## Development


#### Dead simple setup

Get yourself the [Elm programming language](http://elm-lang.org/):

On node 6+: `npm i -g elm`

Then you can just do `elm-make src/Main.elm --output=elm.js --debug` and open `index.html`.

#### Nicer workflow

Use `elm-live` (`npm i -g elm-live`)

```
elm-live src/Main.elm --output=elm.js --open --debug
```

This will open a browser tab with CSS hot reloading and page refreshing on Elm code changes.
