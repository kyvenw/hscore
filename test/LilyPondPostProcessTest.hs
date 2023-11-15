module LilyPondPostProcessTest (runTests) where

import Data.List (sort)
import Data.Ratio
import Euterpea.Music (Dur, Mode (Major, Minor), PitchClass (..), hn, qn)
import EuterpeaTypes
import HScoreConfiguration
import LilyPondGen
import LilyPondPostProcess
import LilyPondSyntax
import MusicTypes
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck hiding (quickCheckAll)

runTests :: IO Counts
runTests = do
  quickCheckAll
  runTestTT postProcessTimeKeyTest
  runTestTT breakNoteIntoTieTest
  runTestTT postProcessAccidentalsTest
  runTestTT guessCleffTest
  runTestTT mergeRestsTest
  runTestTT sortStavesByPitchAndNoteCountTest
  runTestTT sortStavesByNoteCountTest
  runTestTT tryCombineMeasuresTest

spec2 :: MusicSpecs
spec2 =
  defaultConfig
    { timeKeySpecs =
        [ TimeKeySpec (KeySig D Major) (TimeSig 4 4) (Just 1),
          TimeKeySpec (KeySig E Minor) (TimeSig 2 4) Nothing
        ]
    }

defaultSection :: [LMeasure] -> Section
defaultSection = Section Bass (KeySig C Major) (TimeSig 4 4)

section2a :: [LMeasure] -> Section
section2a = Section Bass (KeySig D Major) (TimeSig 4 4)

section2b :: [LMeasure] -> Section
section2b = Section Bass (KeySig E Minor) (TimeSig 2 4)

note :: Dur -> LElement
note dur = LNote dur False (C, 1)

rest :: Dur -> LElement
rest = LRest

rawStaff1 = LRawStaff [note qn, rest qn, rest qn, note qn, note qn, note qn, rest qn, note qn]

outStaff1 = Staff [defaultSection [LMeasure [note qn, rest qn, rest qn, note qn], LMeasure [note qn, note qn, rest qn, note qn]]]

outStaff2 = Staff [section2a [LMeasure [note qn, rest qn, rest qn, note qn]], section2b [LMeasure [note qn, note qn], LMeasure [rest qn, note qn]]]

postProcessTimeKeyTest :: Test
postProcessTimeKeyTest =
  TestList
    [ postProcessTimeKey defaultConfig [rawStaff1] ~?= [outStaff1],
      postProcessTimeKey spec2 [rawStaff1] ~?= [outStaff2]
    ]

breakNoteIntoTieTest :: Test
breakNoteIntoTieTest =
  TestList
    [ breakNoteIntoTie (LNote 1 False (C, 3)) (1 % 2) ~?= (LNote hn True (C, 3), LNote hn False (C, 3)),
      breakNoteIntoTie (LRest 1) qn ~?= (LRest qn, LRest (3 % 4))
    ]

dFlatSectionSharps :: Section
dFlatSectionSharps = Section Treble (KeySig Df Major) (TimeSig 4 4) sharpMeasures

dFlatSectionAdjusted :: Section
dFlatSectionAdjusted = Section Treble (KeySig Df Major) (TimeSig 4 4) measuresDfKeyAdjusted

sharpMeasures :: [LMeasure]
sharpMeasures =
  [ LMeasure [LNote qn False (Bs, 3), LNote qn False (Fs, 3), LNote qn False (Cs, 3), LNote qn False (Gs, 3)],
    LMeasure [LNote qn False (Ds, 3), LNote qn False (As, 3), LNote qn False (Es, 3), LRest qn]
  ]

measuresDfKeyAdjusted :: [LMeasure]
measuresDfKeyAdjusted =
  [ LMeasure [LNote qn False (Bs, 3), LNote qn False (Gf, 3), LNote qn False (Df, 3), LNote qn False (Af, 3)],
    LMeasure [LNote qn False (Ef, 3), LNote qn False (Bf, 3), LNote qn False (Es, 3), LRest qn]
  ]

postProcessAccidentalsTest :: Test
postProcessAccidentalsTest =
  TestList
    [ postProcessAccidentals True [Staff [dFlatSectionSharps]] ~?= [Staff [dFlatSectionAdjusted]],
      postProcessAccidentals False [Staff [dFlatSectionSharps]] ~?= [Staff [dFlatSectionSharps]]
    ]

middleCStaff :: LRawStaff
middleCStaff = LRawStaff [c4Note, c4Note, c4Note, c4Note]

lowCStaff :: LRawStaff
lowCStaff = LRawStaff [lowC4Note, lowC4Note, lowC4Note, lowC4Note]

guessCleffTest :: Test
guessCleffTest =
  TestList
    [ guessCleff middleCStaff ~?= Treble,
      guessCleff lowCStaff ~?= Bass
    ]

separateRestsStaff :: Staff
separateRestsStaff = Staff [section [LMeasure (replicate 4 (LRest qn)), LMeasure (replicate 2 (LRest hn))]]

twoEmptyMeasures :: Staff
twoEmptyMeasures = Staff [section (replicate 2 (LMeasure [LRest 1]))]

mergeRestsTest :: Test
mergeRestsTest =
  TestList
    [ mergeRests [separateRestsStaff] ~?= [twoEmptyMeasures]
    ]

staff8Notes :: Staff
staff8Notes = Staff [section [cm34, cm34]]

staff4Notes :: Staff
staff4Notes = Staff [section [cm34, emptyMeasure]]

staff4NotesHigh :: Staff
staff4NotesHigh = Staff [section [LMeasure $ replicate 4 (LNote qn False (B, 6)), emptyMeasure]]

section :: [LMeasure] -> Section
section = Section Treble (KeySig C Major) (TimeSig 4 4)

sortStavesByPitchAndNoteCountTest :: Test
sortStavesByPitchAndNoteCountTest =
  TestList
    [ (reverse . sort) [staff4Notes, staff8Notes] ~?= [staff8Notes, staff4Notes],
      (reverse . sort) [staff8Notes, staff4NotesHigh] ~?= [staff4NotesHigh, staff8Notes],
      (reverse . sort) [staff4Notes, staff4NotesHigh] ~?= [staff4NotesHigh, staff4Notes]
    ]

sortStavesByNoteCountTest :: Test
sortStavesByNoteCountTest =
  TestList
    [ sortStavesByNoteCount [staff4Notes, staff8Notes] ~?= [staff8Notes, staff4Notes]
    ]

c4Note :: LElement
c4Note = LNote qn False (C, 4)

lowC4Note :: LElement
lowC4Note = LNote qn False (C, 3)

cm1 :: LMeasure
cm1 = LMeasure [LRest (3 % 4), c4Note]

cm2 :: LMeasure
cm2 = LMeasure [c4Note, LRest (3 % 4)]

cm12 :: LMeasure
cm12 = LMeasure [c4Note, LRest hn, c4Note]

cm12Empty :: LMeasure
cm12Empty = LMeasure [LRest qn, LRest (3 % 4)]

cm3 :: LMeasure
cm3 = LMeasure [c4Note, c4Note, c4Note, LRest qn]

cm4 :: LMeasure
cm4 = LMeasure [LRest (3 % 4), c4Note]

cm34 :: LMeasure
cm34 = LMeasure [c4Note, c4Note, c4Note, c4Note]

cm34Empty :: LMeasure
cm34Empty = LMeasure [LRest (3 % 4), LRest qn]

emptyMeasure :: LMeasure
emptyMeasure = LMeasure [LRest 1]

tryCombineMeasuresTest :: Test
tryCombineMeasuresTest =
  TestList
    [ tryCombineMeasures (cm1, cm2) ~?= (cm12, cm12Empty),
      tryCombineMeasures (cm3, cm4) ~?= (cm34, cm34Empty),
      tryCombineMeasures (emptyMeasure, cm4) ~?= (cm4, cm34Empty)
    ]

prop_break_note_into_tie :: LElement -> SmallDur -> Property
prop_break_note_into_tie elt (SmallDur remaining) =
  remaining < elementDur elt ==> case elt of
    LRest dur ->
      let (a, b) = breakNoteIntoTie elt remaining
       in a == LRest remaining && b == LRest (dur - remaining)
    LNote dur tie p ->
      let (a, b) = breakNoteIntoTie elt remaining
       in a == LNote remaining True p && b == LNote (dur - remaining) False p

quickCheckAll :: IO ()
quickCheckAll = do
  quickCheck prop_break_note_into_tie
