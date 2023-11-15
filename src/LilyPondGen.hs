-- http://lilypond.org/doc/v2.18/Documentation/notation/creating-midi-files
module LilyPondGen where

import Data.List (tails)
import Data.Ratio ((%))
import Euterpea.Music
import EuterpeaTypes
import LilyPondSyntax
import MusicGen
import MusicTypes
import Test.QuickCheck

newtype GeneratedRawStaves = GeneratedRawStaves {staves :: [LRawStaff]}
  deriving (Show, Eq)

genStaff :: Int -> Gen LRawStaff
genStaff 1 = do
  note <- arbitrary :: Gen LElement
  return $ LRawStaff [note]
genStaff n = do
  (LRawStaff staff1) <- arbitrary
  (LRawStaff staff2) <- genStaff (n - 1)
  return $ LRawStaff $ staff1 ++ staff2

instance Arbitrary LRawStaff where
  arbitrary = resize 10 $ sized genStaff
  shrink _ = []

genStaves :: Int -> Gen GeneratedRawStaves
genStaves 1 = do
  base <- arbitrary
  return $ GeneratedRawStaves [base]
genStaves n = do
  base <- arbitrary
  (GeneratedRawStaves l) <- genStaves (n - 1)
  return $ GeneratedRawStaves (base : l)

instance Arbitrary GeneratedRawStaves where
  arbitrary = resize 3 $ sized genStaves
  shrink _ = []

instance Arbitrary LElement where
  arbitrary =
    frequency
      [ (3, LNote <$> genDur <*> arbitrary <*> genPitch),
        (1, LRest <$> genDur)
      ]
    where
      genPitch = (,) <$> arbitrary <*> elements [1 .. 10]
      genDur = elements [1, 1 / 2, 1 / 4]
  shrink _ = []

newtype SmallDur = SmallDur {d :: Dur}
  deriving (Show, Eq)

instance Arbitrary SmallDur where
  arbitrary = oneof $ map (return . SmallDur) [1, 1 % 2, 1 % 4, 1 % 8, 1 % 16]