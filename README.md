diagrams-svg [![Hackage](https://img.shields.io/hackage/v/diagrams-svg.svg?style=flat)](https://hackage.haskell.org/package/diagrams-svg) [![Build Status](https://travis-ci.org/diagrams/diagrams-svg.png?branch=master)](http://travis-ci.org/diagrams/diagrams-svg)
------------


_diagrams-svg_ is a an SVG backend for [diagrams]. Diagrams is a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell

_diagrams-svg_ is the default out-of-the box backend that comes with
the diagrams framework, and supports most features defined in [diagrams-lib].

[diagrams-lib]: http://hackage.haskell.org/package/diagrams%2Dlib

# Installation

```
cabal update && cabal install diagrams-svg
```

# Usage

A simple example that uses _diagrams-svg_ to draw a square.

```haskell
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

b1 = square 20 # lw 0.002

main = mainWith (pad 1.1 b1)
```

Save this to file named `Square.hs` and compile this program:

```
ghc --make Square.hs
```

This will generate an executable which, when run produces an SVG file. Run the
executable with the `--help` option to find out more about how to call it.

```
$ ./Square --help
./Square

Usage: ./Square [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT] [--loop] [-s|--src ARG] [-i|--interval INTERVAL]
  Command-line diagram generation.

Available options:
  -?,--help                Show this help text
  -w,--width WIDTH         Desired WIDTH of the output image
  -h,--height HEIGHT       Desired HEIGHT of the output image
  -o,--output OUTPUT       OUTPUT file
  -l,--loop                Run in a self-recompiling loop
  -s,--src ARG             Source file to watch
  -i,--interval INTERVAL   When running in a loop, check for changes every INTERVAL seconds.
  -p,--pretty              Pretty print the SVG output
```

You _must_ pass an output file name with a `.svg` extension to generate the SVG
file.

```
$ ./Square -o square.svg
```

The command above generates the SVG file:

```
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
    "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="22.0" height="22.0" viewBox="0 0 22 22">
  <g>
    <g stroke="rgb(0,0,0)" stroke-opacity="1.0" fill="rgb(0,0,0)" fill-opacity="0.0" stroke-width="2.0e-3">
       <path d="M 21.0,21.0 l -2.220446049250313e-15,-20.0 h -20.0 l -2.220446049250313e-15,20.0 Z" />
    </g>
  </g>
</svg>
```
