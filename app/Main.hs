module Main where

import Data.Yaml
import Euterpea
import EuterpeaConverter
import EuterpeaTypes
import HScoreConfiguration
import LilyPondPostProcess
import LilyPondPrinter (pretty)
import LilyPondSyntax
import MidiMusicProcessing (readMidi)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (FilePath, dropExtension, takeFileName, (</>))
import System.Process (callCommand)

-- This will read a MIDI file and output a .ly file for LilyPond
main :: IO ()
main = do
  args <- getArgs
  case args of
    [midi] -> action midi (dropExtension midi <> ".yaml")
    [midi, config] -> action midi config
    _ -> do
      putStrLn "MIDI file you would like to convert:"
      midi <- getLine
      putStrLn "Config file you would like to use (default: C Major 4/4):"
      config <- getLine
      action midi config

getConfig :: FilePath -> IO MusicSpecs
getConfig fp = do
  config <- decodeFileEither fp
  case config of
    Left e -> do
      print e
      putStrLn "Defaulting to C Major 4/4"
      return defaultConfig
    Right c -> do
      putStrLn "Found config:"
      print c
      return c

action :: FilePath -> FilePath -> IO ()
action midiFile configFile = do
  let name = dropExtension midiFile
  music <- readMidi midiFile
  lp <- convertEMusicToLilyPond name configFile music
  return ()

convertEMusicToLilyPond :: FilePath -> FilePath -> EMusic -> IO LilyPond
convertEMusicToLilyPond name config emusic = do
  specs <- getConfig config
  let lilypondRep = toLilyPond emusic specs
  runLilyPond name lilypondRep
  return lilypondRep

runLilyPond :: FilePath -> LilyPond -> IO ()
runLilyPond path lp = do
  let name = takeFileName path
  let path' = path <> "_out" </> name
  let ly = path' <> ".ly"
  let pdf = path' <> ".pdf"
  createDirectoryIfMissing True (path <> "_out")
  writeFile ly (pretty lp)
  callCommand $ "lilypond -s -o" ++ path' ++ " " ++ ly
  putStrLn $ "We have generated " ++ ly ++ " and " ++ pdf