{- Converter module from Euterpea to LilyPond Syntax -}

-- module EuterpeaConverter (toLilyPond) where
module EuterpeaConverter where

import qualified Control.Monad as Monad
import Data.List (sort)
import Data.Ratio (denominator, numerator, (%))
import Euterpea.Music
import EuterpeaTypes
import HScoreConfiguration
import LilyPondPostProcess
import LilyPondSyntax
import MusicTypes

-- Top-level converter function for Euterpea music, given a config
toLilyPond :: EMusic -> MusicSpecs -> LilyPond
toLilyPond music specs =
  flip LilyPond specs
    . reverse
    . sort
    . cleanStaves
    . squishStaves
    . sortStavesByNoteCount
    . mergeRests
    . postProcessAccidentals (autoAdjustAccidental specs)
    . postProcessTimeKey specs
    . getRawStaves
    $ music

-- Takes in EMusic and returns a list of raw staves (no formatting, just notes)
getRawStaves :: EMusic -> [LRawStaff]
getRawStaves m = appendRests (helper m 0) (lengthEMusic m)
  where
    helper (m1 :+: m2) n =
      let l1 = lengthEMusic m1
          l2 = lengthEMusic m2
       in joinStaves (helper m1 n) (helper m2 (n + l1)) n l1 l2
    helper (m1 :=: m2) n = helper m1 n ++ helper m2 n
    helper (Prim p) _ = [LRawStaff [toElement p]]
    helper (Modify _ m) n = helper m n

toElement :: EPrimitive -> LElement
toElement p = case p of
  Note dur p -> LNote dur False p
  Rest dur -> LRest dur

{- Combine staves horizontally
   requires the start beat, length of the longest staff in first stack,
   and the length of the longest staff in second stack
-}
joinStaves :: [LRawStaff] -> [LRawStaff] -> Dur -> Dur -> Dur -> [LRawStaff]
joinStaves [] [] _ _ _ = []
joinStaves staff1s [] _ l1 l2 = appendRests staff1s (l1 + l2)
joinStaves [] staff2s _ l1 _ = prependRests staff2s l1
joinStaves (staff1 : staff1s) (staff2 : staff2s) n l1 l2 =
  padStaff l1 staff1 <> padStaff l2 staff2 : joinStaves staff1s staff2s n l1 l2

{- Helper functions to prepend a certain number of rest beats to a raw staff -}
prependRests :: [LRawStaff] -> Dur -> [LRawStaff]
prependRests staves beats = Prelude.map (prepend beats) staves
  where
    prepend beats l = if beats == 0 then l else rests beats <> l

{- Helper functions to append rests to a raw staff to reach a certain length -}
appendRests :: [LRawStaff] -> Dur -> [LRawStaff]
appendRests staves beats = Prelude.map (padStaff beats) staves

{- Pad the end of a staff until the staff reaches a certain length -}
padStaff :: Dur -> LRawStaff -> LRawStaff
padStaff dur l =
  if (dur - lengthRawStaff l) <= 0
    then l
    else l <> rests (dur - lengthRawStaff l)

-- Create a raw staff containing a rest of length d
rests :: Dur -> LRawStaff
rests d = LRawStaff [LRest d]
