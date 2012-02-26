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
-----------------------------------------------------------------------------

module Diagrams.Backend.SVG.CmdLine
       ( defaultMain
       , multiMain
       ) where

import qualified Data.ByteString.Lazy as BS
import qualified Blaze.ByteString.Builder as B

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.SVG

import System.Console.CmdArgs.Implicit hiding (args)

import Prelude hiding      (catch)

import Data.Maybe          (fromMaybe)
import Control.Monad       (when)
import Data.List.Split

import System.Environment  (getArgs, getProgName)
import System.Directory    (getModificationTime)
import System.Process      (runProcess, waitForProcess)
import System.IO           (openFile, hClose, IOMode(..),
                            hSetBuffering, BufferMode(..), stdout)
import System.Exit         (ExitCode(..))
import System.Time         (ClockTime, getClockTime)
import Control.Concurrent  (threadDelay)
import Control.Exception   (catch, SomeException(..), bracket)

#ifdef CMDLINELOOP
import System.Posix.Process (executeFile)
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
             &= help "Desired width of the output image (default 400)"

  , height = def
             &= typ "INT"
             &= help "Desired height of the output image (default 400)"

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

               build = renderDia SVG (SVGOptions (output opts) sizeSpec) d
           BS.writeFile (output opts) (B.toLazyByteString build)
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

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
waitForChange :: Maybe ClockTime -> DiagramOpts -> String -> [String] -> IO ()
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
recompile :: Maybe ClockTime -> String -> Maybe String -> IO (Bool, Maybe ClockTime)
recompile lastAttempt prog mSrc = do
  let errFile = prog ++ ".errors"
      srcFile = fromMaybe (prog ++ ".hs") mSrc
  binT <- maybe (getModTime prog) (return . Just) lastAttempt
  srcT <- getModTime srcFile
  if (srcT > binT)
    then do
      putStr "Recompiling..."
      status <- bracket (openFile errFile WriteMode) hClose $ \h ->
        waitForProcess =<< runProcess "ghc" ["--make", srcFile]
                           Nothing Nothing Nothing Nothing (Just h)

      if (status /= ExitSuccess)
        then putStrLn "" >> putStrLn (replicate 75 '-') >> readFile errFile >>= putStr
        else putStrLn "done."

      curTime <- getClockTime
      return (status == ExitSuccess, Just curTime)

    else return (False, Nothing)

 where getModTime f = catch (Just <$> getModificationTime f)
                            (\(SomeException _) -> return Nothing)
#endif