> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine

We can use the [`colour`
library](http://hackage.haskell.org/package/colour) to generate
successively lighter shades of blue:

> colors = iterate (blend 0.1 white) blue

An order-0 pentaflake is just a pentagon:

> pentaflake' 0 = pentagon 1 # lw 0

An [order-n pentaflake](http://mathworld.wolfram.com/Pentaflake.html) is
an order-(n-1) pentaflake surrounded by five more. The `appends`
function is useful here for positioning the five pentaflakes around the
central one.

> pentaflake' n = appends (p' # fc (colors !! (n-1)))
>                        (zip vs (repeat (rotateBy (1/2) p')))
>   where vs = take 5 . iterate (rotateBy (1/5))
>                     . (if odd n then negated else id) $ unitY
>         p' = pentaflake' (n-1)
>
> pentaflake n = pentaflake' n # fc (colors !! n)

An order-4 pentaflake looks nice. Of course there's an exponential
blowup in the number of primitives, so generating higher-order
pentaflakes can take a long time!

> example = pentaflake 4
> main = defaultMain (pad 1.1 example)
