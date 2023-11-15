module MidiMusicProcessing (readMidi) where

import Data.Ratio
import Euterpea
  ( Music (Modify, (:+:), (:=:)),
    Pitch,
    Primitive (..),
    Volume,
    importFile,
    mMap,
    removeZeros,
  )
import Euterpea.IO.MIDI.FromMidi2
import EuterpeaConverter (toLilyPond)
import EuterpeaTypes
import GHC.Real (infinity)
import HScoreConfiguration
import LilyPondSyntax (LRawStaff, LilyPond (music), Staff)

-- tries to read a midi file into a cleaned EMusic type
readMidi :: FilePath -> IO EMusic
readMidi f = do
  imported <- importFile f
  case imported of
    Left e -> error e
    Right midi -> return $ clean $ fromMidi2 midi

stripControl :: Music (Pitch, Volume) -> Music (Pitch, Volume)
stripControl (Modify _ m) = stripControl m
stripControl (m1 :+: m2) = stripControl m1 :+: stripControl m2
stripControl (m1 :=: m2) = stripControl m1 :=: stripControl m2
stripControl x = x

mapMusic :: Music (Pitch, Volume) -> EMusic
mapMusic = mMap fst

convertPrim :: Primitive (Pitch, Volume) -> EPrimitive
convertPrim (Rest dur) = Rest dur
convertPrim (Note dur (p, _)) = Note dur p

clean :: Music (Pitch, Volume) -> EMusic
clean = removeZeros . mapMusic . stripControl