{- LilyPond Syntax representation

  - Currently unsupported features:
    - shorthand for chords
    - aliases (i.e. variables)

  - NOTE: for Note, PitchClass, and key Mode we are still using Euterpea's primitive API
-}
{-# LANGUAGE FlexibleInstances #-}

module LilyPondSyntax where

-- Imports

import Data.Ord (comparing)
import Data.Ratio
import Data.Yaml
import Euterpea.Music
import EuterpeaTypes
import HScoreConfiguration (MusicSpecs)
import MusicTypes

{- Top-level LilyPond representation -}
data LilyPond = LilyPond {music :: [Staff], specs :: MusicSpecs}
  deriving (Eq, Show)

{- Staff abstraction -}
newtype Staff = Staff [Section]
  deriving (Eq, Show)

instance Ord Staff where
  compare s1 s2 =
    let ((p1, o1), (p2, o2)) = (getFirstNoteInStaff s1, getFirstNoteInStaff s2)
     in case compare o1 o2 of
          GT -> GT
          LT -> LT
          EQ -> case compare p1 p2 of
            GT -> GT
            LT -> LT
            EQ -> compare (countTotalNoteDur s1) (countTotalNoteDur s2)

instance Semigroup Staff where
  (Staff s1) <> (Staff s2) = Staff $ s1 <> s2

instance Monoid Staff where
  mappend = (<>)
  mempty = Staff []

{- Section abstraction -}
data Section = Section
  { clef :: Clef,
    key :: KeySig,
    time :: TimeSig,
    measures :: [LMeasure]
  }
  deriving (Eq, Show)

newtype LMeasure = LMeasure [LElement]
  deriving (Eq, Show)

{- May break "measure" invariant -}
instance Semigroup LMeasure where
  LMeasure r1 <> LMeasure r2 = LMeasure $ r1 <> r2

instance Monoid LMeasure where
  mappend = (<>)
  mempty = LMeasure []

type Tie = Bool

data LElement = LNote Dur Tie Pitch | LRest Dur
  deriving (Eq, Show)

{- Useful property functions -}
lengthStaff :: Staff -> Dur
lengthStaff (Staff xs) = sum $ map lengthSection xs

lengthSection :: Section -> Dur
lengthSection (Section _ _ _ ms) = sum $ map measureDur ms

measureDur :: LMeasure -> Dur
measureDur (LMeasure xs) = sum $ map elementDur xs

elementDur :: LElement -> Dur
elementDur x = case x of
  LNote dur _ _ -> dur
  LRest dur -> dur

{- Functions to facilitate for sorting of staves -}
getFirstNoteInStaff :: Staff -> Pitch
getFirstNoteInStaff (Staff (sec : secs)) = getFirstNoteInSec sec
getFirstNoteInStaff (Staff []) = error "invalid, empty staff"

getFirstNoteInSec :: Section -> Pitch
getFirstNoteInSec (Section _ _ _ measures) = getFirstNoteInMeasures measures

getFirstNoteInMeasures :: [LMeasure] -> Pitch
getFirstNoteInMeasures (LMeasure eles : ms) =
  case getFirstNoteAmongElements eles of
    Nothing -> getFirstNoteInMeasures ms
    (Just pc) -> pc
getFirstNoteInMeasures [] = error "invalid, all sections are nonempty"

getFirstNoteAmongElements :: [LElement] -> Maybe Pitch
getFirstNoteAmongElements [e] = case e of
  LRest _ -> Nothing
  LNote _ _ p -> Just p
getFirstNoteAmongElements (e : es) = case e of
  LRest _ -> getFirstNoteAmongElements es
  LNote _ _ p -> Just p
getFirstNoteAmongElements [] = Nothing

{- For ordering based on how crowded a staff is -}
countTotalNoteDur :: Staff -> Integer
countTotalNoteDur (Staff secs) = foldr sectionNoteDurAcc 0 secs
  where
    sectionNoteDurAcc :: Section -> Integer -> Integer
    sectionNoteDurAcc (Section _ _ _ measures) c =
      c + foldr measureNoteDurAcc 0 measures

    measureNoteDurAcc :: LMeasure -> Integer -> Integer
    measureNoteDurAcc (LMeasure eles) c =
      c + foldr elementIsNoteCount 0 eles

    elementIsNoteCount :: LElement -> Integer -> Integer
    elementIsNoteCount LNote {} c = c + 1
    elementIsNoteCount LRest {} c = c

{- Lower-level "raw" unformatted staves, a list of notes -}
newtype LRawStaff = LRawStaff [LElement]
  deriving (Eq, Show)

instance Semigroup LRawStaff where
  LRawStaff r1 <> LRawStaff r2 = LRawStaff $ r1 <> r2

instance Monoid LRawStaff where
  mappend = (<>)
  mempty = LRawStaff []

lengthRawStaff :: LRawStaff -> Dur
lengthRawStaff (LRawStaff l) = sum $ map elementDur l