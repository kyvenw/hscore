-- Module containing constants and constraints that we use throughout the code
{-# LANGUAGE StandaloneDeriving #-}

module MusicTypes where

import Euterpea.Music
import EuterpeaTypes

-- Derive Read instance for Mode
deriving instance Read Mode => Read Mode

data TimeSig = TimeSig {num :: Integer, denom :: Integer}
  deriving (Eq, Show, Read)

data KeySig = KeySig
  { keyPitch :: PitchClass,
    keyMode :: Mode
  }
  deriving (Eq, Show, Read)

data Clef = Treble | Alto | Tenor | Bass
  deriving (Eq, Show, Read)
