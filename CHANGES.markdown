1.1.0.3 (07 December 2014)

- Allow `JuicyPixels-3.2`

1.1.0.2 (17 November 2014)
--------------------------

- Allow `lens-4.6`

1.1.0.1 (22 August 2014)
------------------------

- Allow `lens-4.4`

1.1 (27 May 2014)
-----------------

* **New features**

    - Support for radial and linear gradients.

    - Support for embedded images in `.png` format.

* **New instances**

    - `Renderable` instances for `DImage Embedded`.

* **API changes**

    - Updates to work with `Measure` units.

    - Substantial refactoring of `Backend` instance to support changes in
      `Diagrams.Core`.

* **Dependency/version changes**

    - New dependencies: `base64-bytestring` and `JuicyPixels`.
    - Allow `lens-4.2`
    - Allow `mtl-2.2`

1.0.2.1 (19 March 2014)
----------------------

  - Allow `lens-4.1`

1.0.2 (8 March 2014)
--------------------

* **New features**

    - Support for including hyperlinks.

* **Dependency/version changes**

    - Allow `diagrams-core-1.1` and `diagrams-lib-1.1`
    - Allow `lens-4.0`

* **Bug fixes**


    - Use `splitFills` to properly render certain diagrams with mixed
      lines and filled loops.  Previously, in certain situations loops that should
      have been filled were not.  ([#43](https://github.com/diagrams/diagrams-svg/issues/43))

    - Don't emit last segment of a loop if it is linear.

      See [diagrams-cairo#38](http://github.com/diagrams/diagrams-cairo/issues/38).  This wasn't actually causing any
      observable problems in the SVG backend output, but this seems a
      better/more robust way to do things in any case.

1.0.1.3 (6 February 2014)
-------------------------

    - require diagrams-lib >= 1.0.1 (for Hashable SizeSpec2D instance)

1.0.1.2 (4 February 2014)
-------------------------

    - Allow `blaze-markup-0.6`

1.0.1.1 (30 January 2014)
-------------------------

    - Work around a bug in GHC 7.4.2, which chokes when deriving Generic
      instances for associated data types.

1.0.1 (26 January 2014)
-----------------------

    - Add `Hashable (Options SVG R2)` instance
    - Remove `Show (Options SVG R2)` instance

    According to the PVP, these changes should require a major version
    bump.  However, a major version bump would be quite annoying and I
    don't expect these instance changes to affect anyone (the changes
    were made for internal reasons).  Please yell if it does affect
    you.

1.0 (24 November 2013)
----------------------

    - Re-implement via new backend `RTree` interface, leading to
      smaller output files.
    - Use new command-line interface from `diagrams-lib`.
    - Export `B` as an alias for `SVG` token

0.8.0.2 (26 October 2013)
-------------------------

    - Documentation improvements

0.8.0.1: 11 September 2013
--------------------------

    - require diagrams-lib-0.7.1

0.8: 10 September 2013 [BROKEN]
-------------------------------

* **New features**

    - Extra SVG definitions, to be inserted in the output, may be
      passed as an argument
    - Support for new miter limit attribute
    - Approximate text alignment

* **Bug fixes**

    - Stacking multiple clip regions now works properly

0.7: 9 August 2013
------------------

* **New features**

    - New `renderToSVG` convenience function
    - Vastly improved Haddock documentation

* **New instances**

    - `Show` instance for `Options SVG R2`

* **Dependency/version changes**
    - allow `base-4.7` and `unix-2.7`
    - Upgrade to `monoid-extras-0.3`

0.6.0.1: 14 December 2012
-------------------------

* Fix link to README on Hackage page

0.6: 11 December 2012
---------------------

First "officially supported" release.

Features still not implemented:

- text alignment
- inline images

As of this release everything else Should Work (tm).
