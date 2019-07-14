# G.O.A.T. (Graphic Ornamentation and Annotation Tool) ![Travis](https://travis-ci.org/thebritican/goat.svg?branch=master)

<p align="center">
 <img src="https://github.com/thebritican/goat/blob/master/docs/images/logo.png" alt="G.O.A.T. Logo"/>
</p>

## Demo
<p align="center">
 <img src="https://cloud.githubusercontent.com/assets/3099999/25921130/28d9c5ea-3589-11e7-9292-aad812cc9a85.gif" alt="G.O.A.T. Demo"/>
</p>

## Motivation
I wanted to help Support Agents give more context to their screenshots!

https://www.zendesk.com/apps/goat/

## Credits

Epic 🐐-ing to [Alan Hogan](https://github.com/alanhogan) for the acronym behind the 🐐, some bugfixes, and more icons!

👏 Huge thanks to [Jian Wei Liau](https://twitter.com/madebyjw) for some beautiful icons and logo! 👏


## Development


#### Dead simple setup

Get yourself the [Elm programming language](http://elm-lang.org/):

On node 6+: `npm install -g elm && npm install`

Then you can just do `elm make src/Main.elm --output=elm.js` and open `index.html`.

#### Nicer workflow

Use `elm-live` (`npm install -g elm-live`) and then run `npm run dev`.

This will open a browser tab with CSS hot reloading and page refreshing on Elm code changes.

#### Working on the Zendesk App

`npm install` will install the necessary dev tools.

`npm start` will run a development server, you can preview your changes on Zendesk by adding the query string: `?zat=true` to the end of the Zendesk Support url.

#### Testing

Setup for first time Elm testers:

`npm i -g elm-test`

Use `npm test` to run the `elm-test` unit, fuzz, and view tests.

## Contributing

This project is welcome to any PRs, first time Elm programmer or not!

If it's a change requiring a decent amount of work, let's chat first!

DM me on [The Elm Language Slack](https://elmlang.herokuapp.com) (**@greg.ziegan**)

Or, make an issue!
