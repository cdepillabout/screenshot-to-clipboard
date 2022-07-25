
# `screenshot-to-clipboard`

[![CI](https://github.com/cdepillabout/screenshot-to-clipboard/actions/workflows/ci.yaml/badge.svg)](https://github.com/cdepillabout/screenshot-to-clipboard/actions/workflows/ci.yaml)
[![Hackage](https://img.shields.io/hackage/v/screenshot-to-clipboard.svg)](https://hackage.haskell.org/package/screenshot-to-clipboard)
[![Stackage LTS](http://stackage.org/package/screenshot-to-clipboard/badge/lts)](http://stackage.org/lts/package/screenshot-to-clipboard)
[![Stackage Nightly](http://stackage.org/package/screenshot-to-clipboard/badge/nightly)](http://stackage.org/nightly/package/screenshot-to-clipboard)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](./LICENSE)

This program provides a simple way to take a screenshot and copy it to your
system clipboard.

When you run `screenshot-to-clipboard`, it internally runs the
[`import`](https://imagemagick.org/script/import.php) program from
[ImageMagick](https://imagemagick.org) to take a screenshot, and then uses
[GTK](https://www.gtk.org/) to copy that screenshot to your system clipboard.

## Installation

The recommended installation procedure is to use Nix.  With Nix installed, just do:

```console
$ git clone git@github.com:cdepillabout/screenshot-to-clipboard
$ cd screenshot-to-clipboard/
$ nix-build
```

You can run this program with the output `result/bin/screenshot-to-clipboard` program.

`screenshot-to-clipboard` is also available in Nixpkgs, starting with
NixOS-22.11 as `haskellPackages.screenshot-to-clipboard`.

It is also possible to build with `cabal` (or `stack`), but you are responsible for
installing necessary system libraries.  You'll likely need both `imagemagick`,
and the development packages for GTK.

## Usage

If you run `screenshot-to-clipboard`, it will turn your cursor into a
cross-hair.  Click and drag to select a portion of the screen to take a
screenshot.  This screenshot will be copied to your system clipboard.  You can
paste it into another application.

`screenshot-to-clipboard` will continue running until you've copied _something
else_ into your system clipboard.  See the long comment in
[`src/ScreenshotToClipboard.hs`](./src/ScreenshotToClipboard.hs) for why this
is necessary.
