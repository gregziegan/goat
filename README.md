# Image Annotation App

![Travis](https://travis-ci.org/thebritican/annotation-editor.svg?branch=master)
![Example](https://cloud.githubusercontent.com/assets/3099999/23642939/5134b788-02b3-11e7-9b38-8b73e5a22b71.png)

Minimally annotate images with common features like:

- Arrows
- Lines
- Rectangles
- Ellipses
- Text Boxes

## Motivation

This will soon be a [Zendesk Editor App](https://www.zendesk.com/apps/directory/#Compose_&_Edit)

## Development


#### Dead simple setup

Get yourself the [Elm programming language](http://elm-lang.org/):

On node 6+: `npm i -g elm`

Then you can just do `elm-make src/Annotator.elm --output=elm.js --debug` and open `index.html`.

#### Nicer workflow

Use `elm-live` (`npm i -g elm-live`)

```
elm-live src/Annotator.elm --output=elm.js --open --debug
```

This will open a browser tab with CSS hot reloading and page refreshing on Elm code changes.
