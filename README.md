# Webdice

This PureScript program is based on the [Pux Starter
App](https://github.com/alexmingoia/pux-starter-app). It contains all
the JS related boilerplate including webpack integration for
hot-reloading. See the [Pux
Guide](https://alexmingoia.github.io/purescript-pux) for more
information on Pux.

## Installation

```sh
git clone git://github.com/mtesseract/webdice.git
cd webdice
npm install
npm start
```

Webdice should then be listening on `http://localhost:3000`.

## Available scripts (copied verbatim from pux-starter-app)

### watch

`npm start` will start a development server, which hot-reloads your
application when sources changes.

### serve

`npm run serve` serves your application without watching for changes or hot-reloading. For
production run `npm run serve:prod`.

### build

`npm run build` bundles and minifies your application to run in production mode.

## License

I hereby publish the code written by me under the BSD3 license. This
license does not necessarily apply to the boilerplate code taken from
the pux-starter-app, including the webpack integration code, which
seems to be licensed under the Apache 2.0 license (according to the
original bower.json).
