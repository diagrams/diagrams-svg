{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstrainedClassMethods   #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- | A combat trying to provide `defaultLoopRender`, `DiagramLoopOpts` that allows for
--  recompilation of the source file and rerendering of the diagram to function
--  after diagrams-lib >= 1.5, code taken from diagrams-lib == 1.47
-----------------------------------------------------------------------------

module Diagrams.Backend.SVG.Combat (defaultLoopRender, DiagramLoopOpts) where

#if MIN_VERSION_base(4,22,0)
import           Diagrams.Backend.CmdLine (defaultLoopRender, DiagramLoopOpts)
#else
import           Diagrams.Backend.CmdLine (Parseable(..))
import           Diagrams.Prelude         hiding (height, interval, output,
                                           width)
import           Options.Applicative
import           Control.Monad             (forever, unless, when)
import           Data.IORef
import           Data.List                 (delete)
import           Data.Maybe                (fromMaybe)
import           Control.Concurrent        (threadDelay)
import           System.Directory          (canonicalizePath)
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (ExitCode (..))
import           System.FilePath           (dropExtension,
                                            replaceExtension,
                                            takeDirectory, takeFileName, (</>))
import           System.FSNotify           (defaultConfig,
                                            eventTime, watchDir,
                                            withManagerConf)
import           System.FSNotify.Devel     (existsEvents)
import           System.Info               (os)
import           System.IO                 (hFlush, stdout)
import           System.Process            (readProcessWithExitCode)



data DiagramLoopOpts = DiagramLoopOpts
  { loop     :: Bool            -- ^ Flag to indicate that the program should loop creation.
  , src      :: Maybe FilePath  -- ^ File path for the source file to recompile.
  }


instance Parseable DiagramLoopOpts where
  parser = diagramLoopOpts

-- | CommandLine parser for 'DiagramLoopOpts'
--   Loop is @--loop@ or @-l@.
--   Source is @--src@ or @-s@.
diagramLoopOpts :: Parser DiagramLoopOpts
diagramLoopOpts = DiagramLoopOpts
  <$> switch (long "loop" <> short 'l' <> help "Run in a self-recompiling loop")
  <*> (optional . strOption)
      ( long "src" <> short 's'
     <> help "Source file to watch")

-- | Display the list of diagrams available for rendering.

-- | @defaultAnimMainRender@ is an implementation of 'mainRender' which renders
--   an animation as numbered frames, named by extending the given output file
--   name by consecutive integers.  For example if the given output file name is
--   @foo\/blah.ext@, the frames will be saved in @foo\/blah001.ext@,
--   @foo\/blah002.ext@, and so on (the number of padding digits used depends on
--   the total number of frames).  It is up to the user to take these images and
--   stitch them together into an actual animation format (using, /e.g./
--   @ffmpeg@).
--
--   Of course, this is a rather crude method of rendering animations;
--   more sophisticated methods will likely be added in the future.
--
--   The @fpu@ option from 'DiagramAnimOpts' can be used to control how many frames will
--   be output for each second (unit time) of animation.
--
--   This function requires a lens into the structure that the particular backend
--   uses for it's diagram base case.  If @MainOpts (QDiagram b v n Any) ~ DiagramOpts@
--   then this lens will simply be 'output'.  For a backend supporting looping
--   it will most likely be @_1 . output@.  This lens is required because the
--   implementation works by modifying the output field and running the base @mainRender@.
--   Typically a backend can write its @Animation B V@ instance as
--
--   @
--   instance Mainable (Animation B V) where
--       type MainOpts (Animation B V) = (DiagramOpts, DiagramAnimOpts)
--       mainRender = defaultAnimMainRender output
--   @
--
--   We do not provide this instance in general so that backends can choose to
--   opt-in to this form or provide a different instance that makes more sense.


putStrF :: String -> IO ()
putStrF s = putStr s >> hFlush stdout

defaultLoopRender :: DiagramLoopOpts -> IO ()
defaultLoopRender opts = when (loop opts) $ do
  putStrLn "Looping turned on"
  prog <- getProgName
  args <- getArgs

  srcPath <- case src opts of
    Just path -> return path
    Nothing   -> fromMaybe (error nosrc) <$> findHsFile prog
      where
        nosrc = "Unable to find Haskell source file.\n"
             ++ "Specify source file with '-s' or '--src'"
  srcPath' <- canonicalizePath srcPath

  sandbox     <- findSandbox []
  sandboxArgs <- case sandbox of
    Nothing -> return []
    Just sb -> do
      putStrLn ("Using sandbox " ++ takeDirectory sb)
      return ["-package-db", sb]

  let args'       = delete "-l" . delete "--loop" $ args
      newProg     = newProgName (takeFileName srcPath) prog
      timeOfDay   = take 8 . drop 11 . show . eventTime

  withManagerConf defaultConfig $
    \mgr -> do
      lock <- newIORef False

      _ <- watchDir mgr (takeDirectory srcPath') (existsEvents (== srcPath'))
        $ \ev -> do
          running <- atomicModifyIORef lock ((,) True)
          unless running $ do
            putStrF ("Modified " ++ timeOfDay ev ++ " ... ")
            exitCode <- recompile srcPath' newProg sandboxArgs
            -- Call the new program without the looping option
            run newProg args' exitCode
            atomicWriteIORef lock False

      putStrLn $ "Watching source file " ++ srcPath
      putStrLn $ "Compiling target: " ++ newProg
      putStrLn $ "Program args: " ++ unwords args'
      forever . threadDelay $ case os of
         -- https://ghc.haskell.org/trac/ghc/ticket/7325
        "darwin" -> 2000000000
        _        -> maxBound

recompile :: FilePath -> FilePath -> [String] -> IO ExitCode
recompile srcFile outFile args = do
  let ghcArgs = ["--make", srcFile, "-o", outFile] ++ args
  putStrF "compiling ... "
  (exit, _, stderr) <- readProcessWithExitCode "ghc" ghcArgs ""
  when (exit /= ExitSuccess) $ putStrLn ('\n':stderr)
  return exit

-- | On Windows, the next compilation must have a different output
--   than the currently running program.
newProgName :: FilePath -> String -> String
newProgName srcFile oldName = case os of
  "mingw32" ->
      if oldName == replaceExtension srcFile "exe"
        then replaceExtension srcFile ".1.exe"
        else replaceExtension srcFile "exe"
  _ -> dropExtension srcFile

-- | Run the given program with specified arguments, if and only if
--   the previous command returned ExitSuccess.
run :: String -> [String] -> ExitCode -> IO ()
run prog args ExitSuccess = do
  let path = "." </> prog
  putStrF "running ... "
  (exit, stdOut, stdErr) <- readProcessWithExitCode path args ""
  case exit of
    ExitSuccess   -> putStrLn "done."
    ExitFailure r -> do
      putStrLn $ prog ++ " failed with exit code " ++ show r
      unless (null stdOut) $ putStrLn "stdout:" >> putStrLn stdOut
      unless (null stdErr) $ putStrLn "stderr:" >> putStrLn stdErr
run _ _ _ = return ()
#endif
