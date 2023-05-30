## [v1.4.3.1-r5](https://github.com/diagrams/diagrams-svg/tree/v1.4.3.1-r5) (2023-05-30)

- Allow `base-4.18`, `mtl-2.3`, `optparse-applicative-0.18`, and test on GHC 9.6

## [v1.4.3.1-r4](https://github.com/diagrams/diagrams-svg/tree/v1.4.3.1-r4) (2022-09-05)

- Allow `base-4.17`, `lens-5.2`, and test on GHC 9.4

## [v1.4.3.1-r3](https://github.com/diagrams/diagrams-svg/tree/v1.4.3.1-r3) (2022-02-02)

- Allow `optparse-applicative-0.17`.

## [v1.4.3.1-r2](https://github.com/diagrams/diagrams-svg/tree/v1.4.3.1-r2) (2022-01-10)

- Allow `text-2.0`.

## [v1.4.3.1](https://github.com/diagrams/diagrams-svg/tree/v1.4.3.1) (2021-12-28)

- Dependency upper bounds updates to allow:
    - `base-4.16` (GHC 9.2)
    - `lens-5.1`
    - `hashable-1.4`
    - `semigroups-0.20`
- Add `Eq` instance for `Options SVG` and (orphan) `Eq` instance for `Element`

## [v1.4.3-r3](https://github.com/diagrams/diagrams-svg/tree/v1.4.3-r3) (2021-06-08)

Dependency upper bounds updates, to allow:

- `base-4.15` (GHC 9.0)
- `base64-bytestring-1.2`
- `diagrams-core-1.5`
- `monoid-extras-0.6`
- `lens-5.0`
- `optparse-applicative-0.16`

## [v1.4.3](https://github.com/diagrams/diagrams-svg/tree/v1.4.3) (2019-12-10)

- Allow `base-4.13` (GHC 8.8), `lens-4.18`, `semigroups-0.19`,
  `hashable-1.3`, `optparse-applicative-0.15`
- Stop rounding the coordinates of the viewbox
  ([#109](https://github.com/diagrams/diagrams-svg/issues/109))
- New `svgClass`, `svgId`, and `svgTitle` functions for setting SVG
  attributes via annotations

## [v1.4.2](https://github.com/diagrams/diagrams-svg/tree/v1.4.2) (2018-05-09)

- Allow `base-4.11` (GHC 8.4) and `lens-4.16`
- Add `Semigroup (Render SVG V2 n)` instance

## [v1.4.1.1](https://github.com/diagrams/diagrams-svg/tree/v1.4.1.1) (2017-08-23)

- Allow `base-4.10` and `optparse-applicative-0.14`
- Fix gradients applied to text ([#98](https://github.com/diagrams/diagrams-svg/issues/98))

## [v1.4.1](https://github.com/diagrams/diagrams-svg/tree/v1.4.1) (2016-10-26)

- Handle wider range of font weight specifications
- allow `lens-4.15`

## [v1.4.0.4](https://github.com/diagrams/diagrams-svg/tree/v1.4.0.4) (2016-08-22)

- Require `optparse-applicative-0.13`, and fix compilation error

## [v1.4.0.3](https://github.com/diagrams/diagrams-svg/tree/v1.4.0.3) (2016-08-16)

- Allow `optparse-applicative-0.13`

## [v1.4.0.2](https://github.com/diagrams/diagrams-svg/tree/v1.4.0.2) (2016-06-06)

- allow `base-4.9`
- test with GHC 8.0
- minor documentation updates

## [v1.4.0.1](https://github.com/diagrams/diagrams-svg/tree/v1.4.0.1) (2016-05-01)

- allow `lens-4.14`

## [v1.4](https://github.com/diagrams/diagrams-svg/tree/v1.4) (2016-02-14)

-- Changes for `svg-builder`
-- Deprecate `svgId` and `svgClass`

## [v1.3.1.8](https://github.com/diagrams/diagrams-svg/tree/v1.3.1.8) (2015-11-14)

- allow `lucid-svg-0.6`

## [v1.3.1.7](https://github.com/diagrams/diagrams-svg/tree/v1.3.1.7) (2015-11-10)

- allow `semigroups-0.18`

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.3.1.6...v1.3.1.7)

## [v1.3.1.6](https://github.com/diagrams/diagrams-svg/tree/v1.3.1.6) (2015-09-29)

- allow `optparse-applicative-0.12`

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.3.1.5...v1.3.1.6)


## [v1.3.1.5](https://github.com/diagrams/diagrams-svg/tree/v1.3.1.5) (2015-09-19)

- allow `lens-4.13` and `semigroups-0.17`

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.3.1.4...v1.3.1.5)

## [v1.3.1.4](https://github.com/diagrams/diagrams-svg/tree/v1.3.1.4) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.3.1.3...v1.3.1.4)

## [v1.3.1.3](https://github.com/diagrams/diagrams-svg/tree/v1.3.1.2) (2015-07-09)

- Changes to allow `lucid-svg-0.5`

## [v1.3.1.2](https://github.com/diagrams/diagrams-svg/tree/v1.3.1.2) (2015-05-26)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.3.1.1...v1.3.1.2)

## [v1.3.1.1](https://github.com/diagrams/diagrams-svg/tree/v1.3.1.1) (2015-05-06)

**Bug Fix**: handle empty dashing array properly ([\#80](https://github.com/diagrams/diagrams-svg/pull/80))

## [v1.3.1](https://github.com/diagrams/diagrams-svg/tree/v1.3.1) (2015-04-30)

**API Changes**

  - Type of `SVGOptions` changed. `_svgDefinitions :: Maybe SvgM`

**Internal Changes**

  - Use `ReaderT` for styles

## [v1.3](https://github.com/diagrams/diagrams-svg/tree/v1.3) (2015-04-19)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.1.0.4...v1.1.0.5)

**New features**

- User settable ID prefixes

- Support opacity group

- Fix clipping bug (Issue #70)

**Internal Changes**

- Switch rendering engine from `blaze-svg` to `lucid-svg`

- Use `fsnotify` for looping

- Add defs tags for clips and gradients

**Implemented enhancements:**

- Put `clip path`, `gradients`, etc in defs tag. [\#73](https://github.com/diagrams/diagrams-svg/issues/73)

**Fixed bugs:**

- Font size not applied properly in composite diagram [\#66](https://github.com/diagrams/diagrams-svg/issues/66)

- clipping broken [\#70](https://github.com/diagrams/diagrams-svg/issues/70)

**Merged pull requests:**

- State [\#74](https://github.com/diagrams/diagrams-svg/pull/74) ([cchalmers](https://github.com/cchalmers))

- Bump base upper bound [\#72](https://github.com/diagrams/diagrams-svg/pull/72) ([bgamari](https://github.com/bgamari))

- Allow user to set ID prefixes [\#71](https://github.com/diagrams/diagrams-svg/pull/71) ([mightybyte](https://github.com/mightybyte))

- Lucid [\#69](https://github.com/diagrams/diagrams-svg/pull/69) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Use fsnotify for looping, via diagrams-lib [\#67](https://github.com/diagrams/diagrams-svg/pull/67) ([bergey](https://github.com/bergey))

## [v1.1.0.5](https://github.com/diagrams/diagrams-svg/tree/v1.1.0.5) (2015-04-03)

- allow `lens-4.9`

- allow `vector-space-0.10`

- allow `blaze-markup-0.7`

## [v1.1.0.4](https://github.com/diagrams/diagrams-svg/tree/v1.1.0.4) (2015-01-13)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.1.0.3...v1.1.0.4)

## [v1.1.0.3](https://github.com/diagrams/diagrams-svg/tree/v1.1.0.3) (2014-12-07)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.1.0.2...v1.1.0.3)

## [v1.1.0.2](https://github.com/diagrams/diagrams-svg/tree/v1.1.0.2) (2014-11-17)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.1.0.1...v1.1.0.2)

**Fixed bugs:**

- Font scaling uses "em" units. [\#30](https://github.com/diagrams/diagrams-svg/issues/30)

**Merged pull requests:**

- Bump lens upper version bounds [\#65](https://github.com/diagrams/diagrams-svg/pull/65) ([RyanGlScott](https://github.com/RyanGlScott))

- New stuff [\#63](https://github.com/diagrams/diagrams-svg/pull/63) ([cchalmers](https://github.com/cchalmers))

- Allows us to write `Diagram B` instead of `Diagram B V2 Double/Float/Whatever` in diagrams programs [\#62](https://github.com/diagrams/diagrams-svg/pull/62) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Linear [\#61](https://github.com/diagrams/diagrams-svg/pull/61) ([cchalmers](https://github.com/cchalmers))

## [v1.1.0.1](https://github.com/diagrams/diagrams-svg/tree/v1.1.0.1) (2014-08-22)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.1...v1.1.0.1)

**Closed issues:**

- Embedding JPEG images without repacking [\#57](https://github.com/diagrams/diagrams-svg/issues/57)

**Merged pull requests:**

- Enable compilation with GHC HEAD \(v7.9\) [\#60](https://github.com/diagrams/diagrams-svg/pull/60) ([ggreif](https://github.com/ggreif))

- Add loadImageSVG to support JPEG-images without repacking via Native DImages [\#58](https://github.com/diagrams/diagrams-svg/pull/58) ([taruti](https://github.com/taruti))

- Allow svg output file to be pretty printed [\#56](https://github.com/diagrams/diagrams-svg/pull/56) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.1](https://github.com/diagrams/diagrams-svg/tree/v1.1) (2014-06-02)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.0.2.1...v1.1)

**New features**

- Support for radial and linear gradients.

- Support for embedded images in `.png` format.

**New instances**

- `Renderable` instances for `DImage Embedded`.

**API changes**

- Updates to work with `Measure` units.

- Substantial refactoring of `Backend` instance to support changes in
  `Diagrams.Core`.

**Dependency/version changes**

- New dependencies: `base64-bytestring` and `JuicyPixels`.

- Allow `lens-4.2`

- Allow `mtl-2.2`

**Closed issues:**

- Support hyperlinks [\#48](https://github.com/diagrams/diagrams-svg/issues/48)

- Line width not handled properly by some browsers [\#33](https://github.com/diagrams/diagrams-svg/issues/33)

- implement image embedding [\#11](https://github.com/diagrams/diagrams-svg/issues/11)

**Merged pull requests:**

- Embedded images - png working [\#54](https://github.com/diagrams/diagrams-svg/pull/54) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- fix text scaling [\#53](https://github.com/diagrams/diagrams-svg/pull/53) ([byorgey](https://github.com/byorgey))

- Updates to work with `Backend` redesign [\#51](https://github.com/diagrams/diagrams-svg/pull/51) ([byorgey](https://github.com/byorgey))

- Rework of units [\#50](https://github.com/diagrams/diagrams-svg/pull/50) ([byorgey](https://github.com/byorgey))

- Preliminary implementation of Measure [\#46](https://github.com/diagrams/diagrams-svg/pull/46) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Gradient [\#41](https://github.com/diagrams/diagrams-svg/pull/41) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

## [v1.0.2.1](https://github.com/diagrams/diagrams-svg/tree/v1.0.2.1) (2014-03-19)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.0.2...v1.0.2.1)

## [v1.0.2](https://github.com/diagrams/diagrams-svg/tree/v1.0.2) (2014-03-09)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.0.1.3...v1.0.2)

**New features**

- Support for including hyperlinks.

**Dependency/version changes**

- Allow `diagrams-core-1.1` and `diagrams-lib-1.1`

- Allow `lens-4.0`

**Bug fixes**


- Use `splitFills` to properly render certain diagrams with mixed
  lines and filled loops.  Previously, in certain situations loops that should
  have been filled were not.  ([#43](https://github.com/diagrams/diagrams-svg/issues/43))

- Don't emit last segment of a loop if it is linear.

  See [diagrams-cairo#38](http://github.com/diagrams/diagrams-cairo/issues/38).  This wasn't actually causing any
  observable problems in the SVG backend output, but this seems a
  better/more robust way to do things in any case.

**Closed issues:**

- SVG backend doesn't fill loops if they occur in the same subtree as a line [\#43](https://github.com/diagrams/diagrams-svg/issues/43)

**Merged pull requests:**

- Hyperlinks [\#49](https://github.com/diagrams/diagrams-svg/pull/49) ([tdox](https://github.com/tdox))

- stop using ignoreFill; use splitFills instead.  Fixes \#43. [\#47](https://github.com/diagrams/diagrams-svg/pull/47) ([byorgey](https://github.com/byorgey))

## [v1.0.1.3](https://github.com/diagrams/diagrams-svg/tree/v1.0.1.3) (2014-02-06)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.0.1.2...v1.0.1.3)

## [v1.0.1.2](https://github.com/diagrams/diagrams-svg/tree/v1.0.1.2) (2014-02-04)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.0.1.1...v1.0.1.2)

## [v1.0.1.1](https://github.com/diagrams/diagrams-svg/tree/v1.0.1.1) (2014-01-30)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.0.1...v1.0.1.1)

## [v1.0.1](https://github.com/diagrams/diagrams-svg/tree/v1.0.1) (2014-01-26)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v1.0...v1.0.1)

**Merged pull requests:**

- Add Hashable instance for Options SVG R2 [\#45](https://github.com/diagrams/diagrams-svg/pull/45) ([byorgey](https://github.com/byorgey))

## [v1.0](https://github.com/diagrams/diagrams-svg/tree/v1.0) (2013-11-25)

According to the PVP, these changes should require a major version
bump.  However, a major version bump would be quite annoying and I
don't expect these instance changes to affect anyone (the changes
were made for internal reasons).  Please yell if it does affect
you.

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v0.8.0.2...v1.0)

**Fixed bugs:**

- SVG backend fills lines again [\#42](https://github.com/diagrams/diagrams-svg/issues/42)

## [v0.8.0.2](https://github.com/diagrams/diagrams-svg/tree/v0.8.0.2) (2013-10-26)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v0.8.0.1...v0.8.0.2)

**Closed issues:**

- Text alignment support [\#17](https://github.com/diagrams/diagrams-svg/issues/17)

- Add a workaround for the Chrome stroke-width 0 bug [\#3](https://github.com/diagrams/diagrams-svg/issues/3)

**Merged pull requests:**

- Lens [\#40](https://github.com/diagrams/diagrams-svg/pull/40) ([jeffreyrosenbluth](https://github.com/jeffreyrosenbluth))

- Updating information about 'Options SVG' [\#39](https://github.com/diagrams/diagrams-svg/pull/39) ([co-dan](https://github.com/co-dan))

## [v0.8.0.1](https://github.com/diagrams/diagrams-svg/tree/v0.8.0.1) (2013-09-11)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v0.8...v0.8.0.1)

## [v0.8](https://github.com/diagrams/diagrams-svg/tree/v0.8) (2013-09-10)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v0.7...v0.8)

**New features**

- Extra SVG definitions, to be inserted in the output, may be
  passed as an argument

- Support for new miter limit attribute

- Approximate text alignment

**Bug fixes**

- Stacking multiple clip regions now works properly

**Merged pull requests:**

- Font embedding changes [\#38](https://github.com/diagrams/diagrams-svg/pull/38) ([jbracker](https://github.com/jbracker))

- Added approximation of text alignment. Better then nothing... [\#36](https://github.com/diagrams/diagrams-svg/pull/36) ([jbracker](https://github.com/jbracker))

## [v0.7](https://github.com/diagrams/diagrams-svg/tree/v0.7) (2013-08-09)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v0.6.0.1...v0.7)

**New features**

- New `renderToSVG` convenience function

- Vastly improved Haddock documentation

**New instances**

- `Show` instance for `Options SVG R2`

**Dependency/version changes**

- allow `base-4.7` and `unix-2.7`

- Upgrade to `monoid-extras-0.3`

**Implemented enhancements:**

- Improve Haddock documentation [\#27](https://github.com/diagrams/diagrams-svg/issues/27)

**Fixed bugs:**

- Lines should not be filled [\#35](https://github.com/diagrams/diagrams-svg/issues/35)

**Closed issues:**

- diagrams-svg.cabal out of sync with core and lib [\#34](https://github.com/diagrams/diagrams-svg/issues/34)

- Add function of type  FilePath -\> SizeSpec2D -\> Diagram -\> IO \(\) [\#28](https://github.com/diagrams/diagrams-svg/issues/28)

**Merged pull requests:**

- General SVG backend cleanup, additional documentation, and new API function [\#29](https://github.com/diagrams/diagrams-svg/pull/29) ([byorgey](https://github.com/byorgey))

## [v0.6.0.1](https://github.com/diagrams/diagrams-svg/tree/v0.6.0.1) (2012-12-14)

[Full Changelog](https://github.com/diagrams/diagrams-svg/compare/v0.6...v0.6.0.1)

## [v0.6](https://github.com/diagrams/diagrams-svg/tree/v0.6) (2012-12-12)

First "officially supported" release.

Features still not implemented:

- text alignment

- inline images

As of this release everything else Should Work (tm).

**Closed issues:**

- Line width does not follow specification? [\#24](https://github.com/diagrams/diagrams-svg/issues/24)

- diagrams-svg doesn't build under directory-1.2 \(and hence GHC-7.6\) [\#20](https://github.com/diagrams/diagrams-svg/issues/20)

- Freezing does not appear to work with the SVG backend [\#19](https://github.com/diagrams/diagrams-svg/issues/19)

- Font family support [\#18](https://github.com/diagrams/diagrams-svg/issues/18)

- Fill color needs to be explicitly specified for text nodes [\#15](https://github.com/diagrams/diagrams-svg/issues/15)

- Implement clipping [\#14](https://github.com/diagrams/diagrams-svg/issues/14)

- Add README, documentation and generate documentation on Hackage [\#13](https://github.com/diagrams/diagrams-svg/issues/13)

- implement text attributes [\#10](https://github.com/diagrams/diagrams-svg/issues/10)

- implement text rendering [\#9](https://github.com/diagrams/diagrams-svg/issues/9)

- implement line cap [\#8](https://github.com/diagrams/diagrams-svg/issues/8)

- implement line dashing [\#7](https://github.com/diagrams/diagrams-svg/issues/7)

- Implement line join attribute [\#6](https://github.com/diagrams/diagrams-svg/issues/6)

- Implement opacity [\#5](https://github.com/diagrams/diagrams-svg/issues/5)

- Implement fill rule [\#4](https://github.com/diagrams/diagrams-svg/issues/4)

- Switch to using blaze-svg combinators [\#2](https://github.com/diagrams/diagrams-svg/issues/2)

- Create a new branch for using blaze-svg [\#1](https://github.com/diagrams/diagrams-svg/issues/1)

**Merged pull requests:**

- Fix for clipping with freeze support. [\#26](https://github.com/diagrams/diagrams-svg/pull/26) ([fryguybob](https://github.com/fryguybob))

- Apply frozen transformations with <g\> element. [\#25](https://github.com/diagrams/diagrams-svg/pull/25) ([cmears](https://github.com/cmears))

- ghc-7.6  [\#23](https://github.com/diagrams/diagrams-svg/pull/23) ([michaelt](https://github.com/michaelt))

- Increase cmdargs upper bound to < 0.10 [\#16](https://github.com/diagrams/diagrams-svg/pull/16) ([byorgey](https://github.com/byorgey))

- Line join + cap [\#12](https://github.com/diagrams/diagrams-svg/pull/12) ([byorgey](https://github.com/byorgey))


\* *This Change Log was automatically generated by (and hand edited) [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*
