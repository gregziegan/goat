# G.O.A.T. ![Travis](https://travis-ci.org/thebritican/goat.svg?branch=master)
### Graphics Ornamentation and Annotation Tool
### a.k.a. Greatest Of All Time

## Draw Lines
![screen recording 2017-04-09 at 02 11 pm](https://cloud.githubusercontent.com/assets/3099999/24841113/f31151be-1d30-11e7-88ba-7d4a384e28ff.gif)

## Draw Shapes
![screen recording 2017-04-09 at 02 13 pm](https://cloud.githubusercontent.com/assets/3099999/24841122/023be50a-1d31-11e7-86d1-6718a6897f2d.gif)


## Draw Spotlights
![screen recording 2017-04-09 at 02 18 pm](https://cloud.githubusercontent.com/assets/3099999/24841124/125ad2fc-1d31-11e7-8b1f-af2b10f5cb14.gif)


## Move, Resize, Alter
![screen recording 2017-04-09 at 02 21 pm](https://cloud.githubusercontent.com/assets/3099999/24841127/29f2df72-1d31-11e7-907f-43dee861d1cf.gif)


## Change Stacking Order, Copy/Cut/Paste
![screen recording 2017-04-09 at 02 28 pm](https://cloud.githubusercontent.com/assets/3099999/24841133/3ae7e368-1d31-11e7-9e45-25db7a31544d.gif)


## Motivation

This will soon be a [Zendesk Editor App](https://www.zendesk.com/apps/directory/#Compose_&_Edit)

## Credits

👏 Huge thanks to [Jian Wei Liau](https://twitter.com/madebyjw) for the beautiful icons and logo! 👏

🐐 Thanks to [Alan Hogan](https://github.com/alanhogan) for the acronym behind the 🐐

## Development


#### Dead simple setup

Get yourself the [Elm programming language](http://elm-lang.org/):

On node 6+: `npm i -g elm && npm i`

Then you can just do `elm-make src/Main.elm --output=elm.js --debug` and open `index.html`.

#### Nicer workflow

Use `elm-live` (`npm i -g elm-live`)

```
elm-live src/Main.elm --output=elm.js --open --debug
```

This will open a browser tab with CSS hot reloading and page refreshing on Elm code changes.


#### Testing

Use `npm test` to run the `elm-test` unit, fuzz, and view tests.

## Contributing

This project is welcome to any PRs, first time Elm programmer or not!

If it's a change requiring a decent amount of work, let's chat first!

DM me on [The Elm Language Slack](https://elmlang.herokuapp.com) (**@greg.ziegan**)

Or, make an issue!
