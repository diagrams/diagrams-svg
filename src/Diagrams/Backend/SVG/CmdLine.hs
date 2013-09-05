{-# LANGUAGE DeriveDataTypeable, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.SVG.CmdLine
-- Copyright   :  (c) 2011 Diagrams team (see LICENSE)
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
-- If you want to generate diagrams programmatically---/i.e./ if you
-- want to do anything more complex than what the below functions
-- provide---you have several options.
--
-- * A simple but somewhat inflexible approach is to wrap up
--   'defaultMain' (or 'multiMain') in a call to
--   'System.Environment.withArgs'.
--
-- * You can use 'Diagrams.Backend.SVG.renderSVG' to render a diagram
--   to a file directly; see "Diagrams.Backend.SVG".
--
-- * A more flexible approach is to directly call 'renderDia'; see
--   "Diagrams.Backend.SVG" for more information.

-----------------------------------------------------------------------------

module Diagrams.Backend.SVG.CmdLine
       ( defaultMain
       , multiMain

       , SVG
       ) where

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.SVG

import System.Console.CmdArgs.Implicit hiding (args)

import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as BS

import Data.Maybe          (fromMaybe)
import Control.Monad       (when)
import Data.List.Split

import System.Environment  (getArgs, getProgName)
import System.Directory    (getModificationTime)
import System.Process      (runProcess, waitForProcess)
import System.IO           (openFile, hClose, IOMode(..),
                            hSetBuffering, BufferMode(..), stdout)
import System.Exit         (ExitCode(..))
import Control.Concurrent  (threadDelay)
import qualified Control.Exception as Exc  (catch,  bracket)
import Control.Exception (SomeException(..))

#ifdef CMDLINELOOP
import System.Posix.Process (executeFile)
#endif


# if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock (UTCTime,getCurrentTime)
type ModuleTime = UTCTime
getModuleTime :: IO  ModuleTime
getModuleTime = getCurrentTime
#else
import System.Time         (ClockTime, getClockTime)
type ModuleTime = ClockTime
getModuleTime :: IO  ModuleTime
getModuleTime = getClockTime
#endif


data DiagramOpts = DiagramOpts
                   { width     :: Maybe Int
                   , height    :: Maybe Int
                   , output    :: FilePath
                   , selection :: Maybe String
#ifdef CMDLINELOOP
                   , loop      :: Bool
                   , src       :: Maybe String
                   , interval  :: Int
#endif
                   }
  deriving (Show, Data, Typeable)

diagramOpts :: String -> Bool -> DiagramOpts
diagramOpts prog sel = DiagramOpts
  { width =  def
             &= typ "INT"
             &= help "Desired width of the output image"

  , height = def
             &= typ "INT"
             &= help "Desired height of the output image"

  , output = def
           &= typFile
           &= help "Output file"

  , selection = def
              &= help "Name of the diagram to render"
              &= (if sel then typ "NAME" else ignore)
#ifdef CMDLINELOOP
  , loop = False
            &= help "Run in a self-recompiling loop"
  , src  = def
            &= typFile
            &= help "Source file to watch"
  , interval = 1 &= typ "SECONDS"
                 &= help "When running in a loop, check for changes every n seconds."
#endif
  }
  &= summary "Command-line diagram generation."
  &= program prog

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
--   options.
defaultMain :: Diagram SVG R2 -> IO ()
defaultMain d = do
  prog <- getProgName
  args <- getArgs
  opts <- cmdArgs (diagramOpts prog False)
  chooseRender opts d
#ifdef CMDLINELOOP
  when (loop opts) (waitForChange Nothing opts prog args)
#endif

chooseRender :: DiagramOpts -> Diagram SVG R2 -> IO ()
chooseRender opts d =
  case splitOn "." (output opts) of
    [""] -> putStrLn "No output file given."
    ps | last ps `elem` ["svg"] -> do
           let sizeSpec = case (width opts, height opts) of
                            (Nothing, Nothing) -> Absolute
                            (Just w, Nothing)  -> Width (fromIntegral w)
                            (Nothing, Just h)  -> Height (fromIntegral h)
                            (Just w, Just h)   -> Dims (fromIntegral w)
                                                       (fromIntegral h)

               build = renderDia SVG (SVGOptions sizeSpec Nothing) d
           BS.writeFile (output opts) (renderSvg build)
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps


-- | @multiMain@ is like 'defaultMain', except instead of a single
--   diagram it takes a list of diagrams paired with names as input.
--   The generated executable then takes an argument specifying the
--   name of the diagram that should be rendered.  This is a
--   convenient way to create an executable that can render many
--   different diagrams without modifying the source code in between
--   each one.
multiMain :: [(String, Diagram SVG R2)] -> IO ()
multiMain ds = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog True)
  case selection opts of
    Nothing  -> putStrLn "No diagram selected."
    Just sel -> case lookup sel ds of
      Nothing -> putStrLn $ "Unknown diagram: " ++ sel
      Just d  -> chooseRender opts d

#ifdef CMDLINELOOP
waitForChange :: Maybe ModuleTime -> DiagramOpts -> String -> [String] -> IO ()
waitForChange lastAttempt opts prog args = do
    hSetBuffering stdout NoBuffering
    go lastAttempt
  where go lastAtt = do
          threadDelay (1000000 * interval opts)
          -- putStrLn $ "Checking... (last attempt = " ++ show lastAttempt ++ ")"
          (newBin, newAttempt) <- recompile lastAtt prog (src opts)
          if newBin
            then executeFile prog False args Nothing
            else go $ getFirst (First newAttempt <> First lastAtt)

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
