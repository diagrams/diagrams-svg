_diagrams-svg_ is a an SVG backend for [diagrams]. Diagrams is a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell

_diagrams-svg_ is a work in progress, and some features are not implemented
yet. However, it is functional enough that [hs-logo] uses it as the backend to
do some pretty cool stuff. Check out the [issues page][issues] to get an idea
of the features that are missing.

[issues]: https://github.com/deepakjois/diagrams-svg/issues

# Installation

```
cabal update && cabal install diagrams-svg
```

# Usage

Here is a simple example that uses _diagrams-svg_ to draw a square.

```haskell
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

b1 = square 20 # lw 0.002

main = defaultMain (pad 1.1 b1)
```

Save this to file named `Square.hs` and compile this program:

```
ghc --make Square.hs
```

This will generate an executable which, when run produces an SVG file. Run the
executable with the `--help` option to find out more about how to call it.

```
$ ./Square --help
Command-line diagram generation.

Square [OPTIONS]

Common flags:
  -w --width=INT    Desired width of the output image
  -h --height=INT   Desired height of the output image
  -o --output=FILE  Output file
  -? --help         Display help message
  -V --version      Print version information
```

You _must_ pass an output file name with a `.svg` extension to generate the SVG
file.

```
$ ./Square -o square.svg
```

Here is the SVG file that is generated.

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

## Other Backends

* [diagrams-cairo](http://hackage.haskell.org/package/diagrams-cairo-0.4)
* [diagrams-postscript](http://patch-tag.com/r/fryguybob/diagrams-postscript)
* [diagrams-canvas](https://github.com/byorgey/diagrams-canvas/)

