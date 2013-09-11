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
