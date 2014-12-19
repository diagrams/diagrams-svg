{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.SVG.CmdLine
-- Copyright   :  (c) 2013 Diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the SVG backend.
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-- * 'multiMain' is like 'defaultMain' but allows for a list of
--   diagrams from which the user can choose one to render.
--
-- * 'mainWith' is a generic form that does all of the above but with
--   a slightly scarier type.  See "Diagrams.Backend.CmdLine".  This
--   form can also take a function type that has a subtable final result
--   (any of arguments to the above types) and 'Parseable' arguments.
--
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * Use a function with 'mainWith'.  This may require making
--   'Parseable' instances for custom argument types.
--
-- * Make a new 'Mainable' instance.  This may require a newtype
--   wrapper on your diagram type to avoid the existing instances.
--   This gives you more control over argument parsing, intervening
--   steps, and diagram creation.
--
-- * Build option records and pass them along with a diagram to 'mainRender'
--   from "Diagrams.Backend.CmdLine".
--
-- * You can use 'Diagrams.Backend.SVG.renderSVG' to render a diagram
--   to a file directly; see "Diagrams.Backend.SVG".
--
-- * A more flexible approach is to directly call 'renderDia'; see
--   "Diagrams.Backend.SVG" for more information.
--
-- For a tutorial on command-line diagram creation see
-- <http://projects.haskell.org/diagrams/doc/cmdline.html>.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.SVG.CmdLine
       (
         -- * General form of @main@
         -- $mainwith

         mainWith

         -- * Supported forms of @main@

       , defaultMain
       , multiMain

         -- * Backend tokens

       , SVG
       , B
       ) where

import           Diagrams.Backend.CmdLine
import           Diagrams.Backend.SVG
import           Diagrams.Prelude               hiding (height, interval, width, output)

import           Control.Lens                   hiding (argument)
import           Options.Applicative            hiding ((<>))
import qualified Options.Applicative            as O ((<>))

import qualified Data.ByteString.Lazy           as BS
import qualified Text.Blaze.Svg.Renderer.Pretty as Pretty
import           Text.Blaze.Svg.Renderer.Utf8   (renderSvg)

import           Data.List.Split

#ifdef CMDLINELOOP
import           Control.Concurrent             (threadDelay)
import           Control.Exception              (SomeException (..))
import qualified Control.Exception              as Exc (bracket, catch)
import           Control.Monad                  (when)
import           Data.Maybe                     (fromMaybe)
import           System.Directory               (getModificationTime)
import           System.Exit                    (ExitCode (..))
import           System.IO                      (BufferMode (..), IOMode (..), hClose,
                                                 hSetBuffering, openFile, stdout)
import           System.Process                 (runProcess, waitForProcess)

import           System.Environment             (getArgs, getProgName)
import           System.Posix.Process           (executeFile)


# if MIN_VERSION_directory(1,2,0)
import           Data.Time.Clock                (UTCTime, getCurrentTime)
type ModuleTime = UTCTime
getModuleTime :: IO  ModuleTime
getModuleTime = getCurrentTime
#else
import System.Time         (ClockTime, getClockTime)
type ModuleTime = ClockTime
getModuleTime :: IO  ModuleTime
getModuleTime = getClockTime
#endif
#endif

-- $mainwith
-- The 'mainWith' method unifies all of the other forms of @main@ and is
-- now the recommended way to build a command-line diagrams program.  It
-- works as a direct replacement for 'defaultMain' or 'multiMain' as well
-- as allowing more general arguments.  For example, given a function that
-- produces a diagram when given an @Int@ and a @'Colour' Double@, 'mainWith'
-- will produce a program that looks for additional number and color arguments.
--
-- > ... definitions ...
-- > f :: Int -> Colour Double -> Diagram SVG V2 Double
-- > f i c = ...
-- >
-- > main = mainWith f
--
-- We can run this program as follows:
--
-- > $ ghc --make MyDiagram
-- >
-- > # output image.svg built by `f 20 red`
-- > $ ./MyDiagram -o image.svg -w 200 20 red


-- | This is the simplest way to render diagrams, and is intended to
--   be used like so:
--
-- > ... definitions ...
-- >
-- > main = defaultMain myDiagram
--
--   Compiling this file will result in an executable which takes
--   various command-line options for setting the size, output file,
--   and so on, and renders @myDiagram@ with the specified options.
--
--   Pass @--help@ to the generated executable to see all available
--   options.  Currently it looks something like
--
-- @
-- ./Program
--
-- Usage: ./Program [-w|--width WIDTH] [-h|--height HEIGHT] [-o|--output OUTPUT] [--loop] [-s|--src ARG] [-i|--interval INTERVAL] [-p|--pretty]
--   Command-line diagram generation.
--
-- Available options:
--   -?,--help                Show this help text
--   -w,--width WIDTH         Desired WIDTH of the output image
--   -h,--height HEIGHT       Desired HEIGHT of the output image
--   -o,--output OUTPUT       OUTPUT file
--   -l,--loop                Run in a self-recompiling loop
--   -s,--src ARG             Source file to watch
--   -i,--interval INTERVAL   When running in a loop, check for changes every INTERVAL seconds.
--   -p,--pretty              Pretty print the SVG output
-- @
--
--   For example, a common scenario is
--
-- @
-- $ ghc --make MyDiagram
--
--   # output image.svg with a width of 400pt (and auto-determined height)
-- $ ./MyDiagram -o image.svg -w 400
-- @

defaultMain :: SVGFloat n => QDiagram SVG V2 n Any -> IO ()
defaultMain = mainWith

newtype PrettyOpt = PrettyOpt {isPretty :: Bool}

prettyOpt :: Parser PrettyOpt
prettyOpt = PrettyOpt <$> switch (long "pretty"
                     O.<> short 'p'
                     O.<> help "Pretty print the SVG output")

instance Parseable PrettyOpt where
  parser = prettyOpt

instance SVGFloat n => Mainable (QDiagram SVG V2 n Any) where
#ifdef CMDLINELOOP
    type MainOpts (QDiagram SVG V2 n Any) = (DiagramOpts, DiagramLoopOpts, PrettyOpt)

    mainRender (opts, loopOpts, pretty) d = do
        chooseRender opts pretty d
        when (loopOpts^.loop) (waitForChange Nothing loopOpts)
#else
    type MainOpts (QDiagram SVG V2 n Any) = (DiagramOpts, PrettyOpt)

    mainRender (opts, pretty) = chooseRender opts pretty
#endif

chooseRender :: SVGFloat n => DiagramOpts -> PrettyOpt -> QDiagram SVG V2 n Any -> IO ()
chooseRender opts pretty d =
  case splitOn "." (opts^.output) of
    [""] -> putStrLn "No output file given."
    ps | last ps `elem` ["svg"] -> do
           let szSpec = fromIntegral <$> mkSizeSpec2D (opts^.width) (opts^.height)
               build  = renderDia SVG (SVGOptions szSpec Nothing) d
           if isPretty pretty
             then writeFile (opts^.output) (Pretty.renderSvg build)
             else BS.writeFile (opts^.output) (renderSvg build)
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

-- | @multiMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams paired with names as input.
--   The generated executable then takes a @--selection@ option
--   specifying the name of the diagram that should be rendered.  The
--   list of available diagrams may also be printed by passing the
--   option @--list@.
--
--   Example usage:
--
-- @
-- $ ghc --make MultiTest
-- [1 of 1] Compiling Main             ( MultiTest.hs, MultiTest.o )
-- Linking MultiTest ...
-- $ ./MultiTest --list
-- Available diagrams:
--   foo bar
-- $ ./MultiTest --selection bar -o Bar.eps -w 200
-- @

multiMain :: SVGFloat n => [(String, QDiagram SVG V2 n Any)] -> IO ()
multiMain = mainWith

instance SVGFloat n => Mainable [(String,QDiagram SVG V2 n Any)] where
    type MainOpts [(String,QDiagram SVG V2 n Any)]
        = (MainOpts (QDiagram SVG V2 n Any), DiagramMultiOpts)

    mainRender = defaultMultiMainRender


#ifdef CMDLINELOOP
waitForChange :: Maybe ModuleTime -> DiagramLoopOpts -> IO ()
waitForChange lastAttempt opts = do
    prog <- getProgName
    args <- getArgs
    hSetBuffering stdout NoBuffering
    go prog args lastAttempt
  where go prog args lastAtt = do
          threadDelay (1000000 * opts^.interval)
          -- putStrLn $ "Checking... (last attempt = " ++ show lastAttempt ++ ")"
          (newBin, newAttempt) <- recompile lastAtt prog (opts^.src)
          if newBin
            then executeFile prog False args Nothing
            else go prog args $ getFirst (First newAttempt <> First lastAtt)

-- | @recompile t prog@ attempts to recompile @prog@, assuming the
--   last attempt was made at time @t@.  If @t@ is @Nothing@ assume
--   the last attempt time is the same as the modification time of the
--   binary.  If the source file modification time is later than the
--   last attempt time, then attempt to recompile, and return the time
--   of this attempt.  Otherwise (if nothing has changed since the
--   last attempt), return @Nothing@.  Also return a Bool saying
--   whether a successful recompilation happened.
recompile :: Maybe ModuleTime -> String -> Maybe String -> IO (Bool, Maybe ModuleTime)
recompile lastAttempt prog mSrc = do
  let errFile = prog ++ ".errors"
      srcFile = fromMaybe (prog ++ ".hs") mSrc
  binT <- maybe (getModTime prog) (return . Just) lastAttempt
  srcT <- getModTime srcFile
  if (srcT > binT)
    then do
      putStr "Recompiling..."
      status <- Exc.bracket (openFile errFile WriteMode) hClose $ \h ->
        waitForProcess =<< runProcess "ghc" ["--make", srcFile]
                           Nothing Nothing Nothing Nothing (Just h)

      if (status /= ExitSuccess)
        then putStrLn "" >> putStrLn (replicate 75 '-') >> readFile errFile >>= putStr
        else putStrLn "done."

      curTime <- getModuleTime
      return (status == ExitSuccess, Just curTime)

    else return (False, Nothing)

 where getModTime f = Exc.catch (Just <$> getModificationTime f)
                            (\(SomeException _) -> return Nothing)
#endif

