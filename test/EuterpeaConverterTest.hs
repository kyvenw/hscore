-- HUnit test suite for our Euterpea-to-LilyPond converter
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module EuterpeaConverterTest (runTests) where

import Data.Ratio (denominator, numerator, (%))
import Euterpea (Dur, hn, qn)
import Euterpea.Music
import EuterpeaConverter
import EuterpeaTypes (EMusic, lengthEMusic)
import HScoreConfiguration
import LilyPondSyntax
import MusicGen
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck (Property, Testable, quickCheck, (===), (==>))

-- Sample LilyPondSyntax Objects
c4Note :: LElement
c4Note = LNote qn False (C, 4)

g4Note :: LElement
g4Note = LNote qn False (G, 4)

a4Note :: LElement
a4Note = LNote qn False (A, 4)

g2Note :: LElement
g2Note = LNote hn False (G, 4)

notesGList2 :: [LElement]
notesGList2 = [g4Note, g4Note]

notesGList4 :: [LElement]
notesGList4 = notesGList2 <> notesGList2

notesCList2 :: [LElement]
notesCList2 = [c4Note, c4Note]

notesCList4 :: [LElement]
notesCList4 = [c4Note, c4Note, c4Note, c4Note]

notesCList8 :: [LElement]
notesCList8 = notesCList4 ++ notesCList4

rawStaves48 :: [LRawStaff]
rawStaves48 = [LRawStaff notesCList4, LRawStaff notesCList8]

rawStaves24 :: [LRawStaff]
rawStaves24 = [LRawStaff notesCList2, LRawStaff notesCList4]

rawStaves44 :: [LRawStaff]
rawStaves44 = [LRawStaff notesCList4, LRawStaff notesCList4]

rawStavesn4r4n8 :: [LRawStaff]
rawStavesn4r4n8 = [LRawStaff (notesCList4 ++ [LRest 1]), LRawStaff notesCList8]

rawStavesn2r2n4n4n4 :: [LRawStaff]
rawStavesn2r2n4n4n4 = [LRawStaff (notesCList2 ++ [LRest hn] ++ notesCList4), LRawStaff (notesCList4 ++ notesCList4)]

rawStavesn4r4n8n8n8 :: [LRawStaff]
rawStavesn4r4n8n8n8 = [LRawStaff (notesCList4 ++ [LRest 1] ++ notesCList8), LRawStaff (notesCList8 ++ notesCList8)]

rawStavesn8n4r4n8n8 :: [LRawStaff]
rawStavesn8n4r4n8n8 = [LRawStaff (notesCList8 ++ notesCList4 ++ [LRest 1]), LRawStaff (notesCList8 ++ notesCList8)]

rawStaves88 :: [LRawStaff]
rawStaves88 = [LRawStaff notesCList8, LRawStaff notesCList8]

rawStaves88r2 :: [LRawStaff]
rawStaves88r2 = [LRawStaff (notesCList8 ++ [LRest hn]), LRawStaff (notesCList8 ++ [LRest hn])]

rawStavesr2n88 :: [LRawStaff]
rawStavesr2n88 = [LRawStaff (LRest hn : notesCList8), LRawStaff (LRest hn : notesCList8)]

{- Sample EMusic -}
simpleMusic1 :: EMusic
simpleMusic1 = line1 [c 4 qn, c 4 qn, g 4 qn, g 4 qn, a 4 qn, a 4 qn, g 4 hn]

parallelMusic1 :: EMusic
parallelMusic1 = (g 4 qn :=: c 4 qn) :+: (g 4 qn :=: c 4 qn) :+: (g 4 qn :=: c 4 qn) :+: (g 4 qn :=: c 4 qn)

simpleMusic1RawStaves :: [LRawStaff]
simpleMusic1RawStaves = [LRawStaff [c4Note, c4Note, g4Note, g4Note, a4Note, a4Note, g2Note]]

parallelMusic1RawStaves :: [LRawStaff]
parallelMusic1RawStaves = [LRawStaff notesGList4, LRawStaff notesCList4]

-- Run all tests
runTests :: IO Counts
runTests = do
  quickCheckAll
  runTestTT testAppendRests
  runTestTT testPrependRests
  runTestTT testJoinStaves
  runTestTT testGetRawStaves
  runTestTT testLengthEMusic

testAppendRests :: Test
testAppendRests =
  TestList
    [ appendRests rawStaves48 2 ~?= rawStavesn4r4n8,
      appendRests rawStaves88 (10 % 4) ~?= rawStaves88r2
    ]

testPrependRests :: Test
testPrependRests =
  TestList
    [ prependRests rawStaves88 (1 % 2) ~?= rawStavesr2n88
    ]

testJoinStaves :: Test
testJoinStaves =
  TestList
    [ joinStaves rawStaves24 rawStaves44 0 1 1 ~?= rawStavesn2r2n4n4n4,
      joinStaves rawStaves48 rawStaves88 0 2 2 ~?= rawStavesn4r4n8n8n8,
      joinStaves rawStaves88 rawStaves48 0 2 2 ~?= rawStavesn8n4r4n8n8
    ]

testGetRawStaves :: Test
testGetRawStaves =
  TestList
    [ getRawStaves simpleMusic1 ~?= simpleMusic1RawStaves,
      getRawStaves parallelMusic1 ~?= parallelMusic1RawStaves
    ]

testLengthEMusic :: Test
testLengthEMusic =
  TestList
    [ lengthEMusic simpleMusic1 ~?= 2,
      lengthEMusic parallelMusic1 ~?= 1
    ]

{- Properties for Euterpea -> RawStaves -}
lengthRawStaves :: [LRawStaff] -> Dur
lengthRawStaves s = maximum (map lengthRawStaff s)

prop_euterpea_rawstaves_length :: EMusic -> Property
prop_euterpea_rawstaves_length m =
  lengthEMusic m === lengthRawStaves (getRawStaves m)

prop_euterpea_join_rawstaves_length :: EMusic -> EMusic -> Property
prop_euterpea_join_rawstaves_length m1 m2 =
  let n1 = lengthEMusic m1
      n2 = lengthEMusic m2
   in (n1 + n2) === lengthRawStaves (joinStaves (getRawStaves m1) (getRawStaves m2) 0 n1 n2)

prop_euterpea_converter_finishes_measures :: EMusic -> Property
prop_euterpea_converter_finishes_measures m =
  let lp@(LilyPond staves _) = toLilyPond m defaultConfig
      l = lengthLilyPond lp
      (n, d) = (numerator l, denominator l)
   in staves /= []
        ==> (n `mod` d == 0) && all ((== l) . lengthStaff) staves
  where
    lengthLilyPond :: LilyPond -> Dur
    lengthLilyPond (LilyPond staves _) = maximum (map lengthStaff staves)

quickCheckAll :: IO ()
quickCheckAll = do
  quickCheck prop_euterpea_rawstaves_length
  quickCheck prop_euterpea_join_rawstaves_length
  quickCheck prop_euterpea_converter_finishes_measures
