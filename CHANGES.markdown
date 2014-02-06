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
