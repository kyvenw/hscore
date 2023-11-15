{-# LANGUAGE LambdaCase #-}

module LilyPondPostProcess where

import Data.List (isSuffixOf, sortBy, sortOn)
import Data.Maybe
import Data.Ord
import Data.Ratio ((%))
import Euterpea hiding (key)
import HScoreConfiguration hiding (measures)
import LilyPondSyntax hiding (key, time)
import qualified LilyPondSyntax as LP
import MusicTypes

{- Given a music specification and raw staves, output list of formatted staves-}
postProcessTimeKey :: MusicSpecs -> [LRawStaff] -> [Staff]
postProcessTimeKey specs [] = []
postProcessTimeKey specs staves =
  map
    ( \musicNotes ->
        formTimeKeySections musicNotes (timeKeySpecs specs) (pickUpDur specs) []
    )
    staves

{- For one staff, apply all specifications and output the formatted staff -}
formTimeKeySections ::
  LRawStaff ->
  [TimeKeySpec] ->
  Maybe Dur ->
  [Section] ->
  Staff
formTimeKeySections notes@(LRawStaff lEles) [] pickup sections =
  let spec = defaultTimeKeySpec
      clef = guessCleff notes
      (leftoverElements, measures) =
        formMeasures
          spec
          lEles
          []
          (firstMeasureDur pickup spec)
          []
   in case leftoverElements of
        [] ->
          Staff $
            sections
              <> [ Section
                     clef
                     (key spec)
                     (time spec)
                     measures
                 ]
        _ -> error "shouldn't happen"
formTimeKeySections
  notes@(LRawStaff lEles)
  [TimeKeySpec key time _]
  pickup
  sections =
    let spec = TimeKeySpec key time Nothing
        clef = guessCleff notes
        (leftoverElements, measures) =
          formMeasures
            spec
            lEles
            []
            (firstMeasureDur pickup spec)
            []
     in case leftoverElements of
          [] -> Staff $ sections <> [Section clef key time measures]
          _ -> error "shouldn't happen"
formTimeKeySections
  notes@(LRawStaff lEles)
  (spec : specs)
  pickup
  sections =
    let clef = guessCleff notes
        (leftoverElements, measures) =
          formMeasures
            spec
            lEles
            []
            (firstMeasureDur pickup spec)
            []
     in case leftoverElements of
          [] ->
            Staff $
              sections
                <> [ Section
                       clef
                       (key spec)
                       (time spec)
                       measures
                   ]
          leftovers ->
            formTimeKeySections
              (LRawStaff leftovers)
              specs
              Nothing
              (sections ++ [Section clef (key spec) (time spec) measures])

timeSigRational :: TimeSig -> Rational
timeSigRational (TimeSig num denom) = num % denom

firstMeasureDur :: Maybe Dur -> TimeKeySpec -> Rational
firstMeasureDur pickup spec = case pickup of
  Nothing -> timeSigRational (time spec)
  (Just beats) -> beats

{- TimeSig -> MusicNotes Left to be put into measures ->
  Current Measure we're buiding -> Beats left in current measure ->
  Final List of Measures
-}
formMeasures ::
  TimeKeySpec ->
  [LElement] ->
  [LElement] ->
  Rational ->
  [LMeasure] ->
  ([LElement], [LMeasure])
formMeasures _ [] currMeasure beatsLeft existingMeasures = ([], existingMeasures)
formMeasures
  spec@(TimeKeySpec _ timeSig measures)
  [e]
  currMeasure
  beatsLeft
  existingMeasures =
    let eleDur = elementDur e
     in if eleDur < beatsLeft || eleDur == beatsLeft
          then ([], existingMeasures <> [finishMeasure currMeasure e beatsLeft])
          else
            let newElement = snd (breakNoteIntoTie e beatsLeft)
             in formMeasures
                  spec
                  [newElement]
                  []
                  (timeSigRational timeSig)
                  (existingMeasures <> [finishMeasure currMeasure e beatsLeft])
formMeasures
  spec@(TimeKeySpec _ timeSig measures)
  (e : es)
  currMeasure
  beatsLeft
  existingMeasures
    | isNothing measures
        || measures > Just (toInteger (length existingMeasures)) =
      let eleDur = elementDur e
       in case compare eleDur beatsLeft of
            LT ->
              formMeasures
                spec
                es
                (currMeasure ++ [e])
                (beatsLeft - eleDur)
                existingMeasures
            EQ ->
              formMeasures
                spec
                es
                []
                (timeSigRational timeSig)
                (existingMeasures ++ [finishMeasure currMeasure e beatsLeft])
            GT ->
              let newElement = snd (breakNoteIntoTie e beatsLeft)
               in formMeasures
                    spec
                    (newElement : es)
                    []
                    (timeSigRational timeSig)
                    ( existingMeasures
                        ++ [finishMeasure currMeasure e beatsLeft]
                    )
    | measures == Just (toInteger (length existingMeasures)) =
      (e : es, existingMeasures)
    | otherwise = error "shouldn't be here"

{- Takes the current notes of the measure we are building,
   the element we want to end the measure with,
   the beats left in the measure,
   and outputs the finished LMeasure
-}
finishMeasure :: [LElement] -> LElement -> Rational -> LMeasure
finishMeasure notesSoFar e beatsLeft
  | elementDur e < beatsLeft =
    LMeasure (notesSoFar <> [e] <> [LRest (beatsLeft - elementDur e)])
  | elementDur e == beatsLeft =
    LMeasure (notesSoFar <> [e])
  | otherwise =
    finishMeasure notesSoFar (fst $ breakNoteIntoTie e beatsLeft) beatsLeft

{- Split a note into two notes -}
breakNoteIntoTie :: LElement -> Rational -> (LElement, LElement)
breakNoteIntoTie (LNote dur _ pitch) newDur =
  (LNote newDur True pitch, LNote (dur - newDur) False pitch)
breakNoteIntoTie (LRest dur) newDur =
  (LRest newDur, LRest (dur - newDur))

{- Process accidentals handler -}
postProcessAccidentals :: Bool -> [Staff] -> [Staff]
postProcessAccidentals b staves =
  if b
    then map adjustAccidentalsStaff staves
    else staves

adjustAccidentalsStaff :: Staff -> Staff
adjustAccidentalsStaff (Staff secs) = Staff (map adjustAccidentalsSection secs)
  where
    adjustAccidentalsSection :: Section -> Section
    adjustAccidentalsSection s =
      s {measures = map (adjustAccidentalsMeasure (LP.key s)) (measures s)}
    adjustAccidentalsMeasure key (LMeasure elements) =
      LMeasure $ map (adjustElementGivenKey key) elements
    adjustElementGivenKey key ele = case ele of
      (LNote d t pitch) -> LNote d t (adjustPitchGivenKey (keyPitch key) pitch)
      (LRest d) -> LRest d

{- Given key, note pitch, returns new note pitch guess -}
adjustPitchGivenKey :: PitchClass -> Pitch -> Pitch
adjustPitchGivenKey keyPitch p@(pitch, o) =
  if ("f" `isSuffixOf` show keyPitch) && ("s" `isSuffixOf` show pitch)
    then getFlatEquivalent p
    else p
  where
    getFlatEquivalent (p, o) = case p of
      Cs -> (Df, o)
      Ds -> (Ef, o)
      Fs -> (Gf, o)
      Gs -> (Af, o)
      As -> (Bf, o)
      _ -> (p, o)

{- Infers the proper cleff of this staff
  (ex: all notes below center c should be bass)
  Choices are Treble and Bass just to be simple and normal
-}
guessCleff :: LRawStaff -> Clef
guessCleff (LRawStaff eles) =
  let (lc, hc) = foldr countElementInOctaveAcc (0, 0) eles
   in if hc > lc
        then Treble
        else Bass
  where
    countElementInOctaveAcc elt (lc, hc) = case elt of
      (LNote _ _ p@(pc, oct)) -> if oct >= 4 then (lc, hc + 1) else (lc + 1, hc)
      (LRest _) -> (lc, hc)

{- Merge all sequential rests in all measures across all staves -}
mergeRests :: [Staff] -> [Staff]
mergeRests = map mergeRestStaff
  where
    mergeRestStaff (Staff secs) = Staff (map mergeRestSection secs)
    mergeRestSection (Section c k t measures) =
      Section c k t (map mergeRestsMeasure measures)
    mergeRestsMeasure (LMeasure eles) = case eles of
      [] -> LMeasure []
      [e] -> LMeasure [e]
      (e1 : e2 : es) -> case (e1, e2) of
        (LRest dur1, LRest dur2) ->
          mergeRestsMeasure (LMeasure $ LRest (dur1 + dur2) : es)
        (_, _) -> LMeasure [e1] <> mergeRestsMeasure (LMeasure $ e2 : es)

{- Purge all empty staves -}
cleanStaves :: [Staff] -> [Staff]
cleanStaves = mergeRests . takeWhile (not . staffAllEmpty)

{- Check if the input staff only contains rests -}
staffAllEmpty :: Staff -> Bool
staffAllEmpty (Staff secs) = all secsEmpty secs
  where
    secsEmpty (Section _ _ _ ms) = all measureEmpty ms
    measureEmpty (LMeasure l) = all (\case LRest _ -> True; _ -> False) l

{- Sort staves by note count in descending order -}
sortStavesByNoteCount :: [Staff] -> [Staff]
sortStavesByNoteCount = sortOn (Down . countTotalNoteDur)

{- Greedy algorithm to try and "squish" notes upwards where possible -}
squishStaves :: [Staff] -> [Staff]
squishStaves [] = []
squishStaves (s : ss) =
  let (resTop, resStaves) = squishOneIntoRest s ss
   in resTop : squishStaves resStaves
  where
    -- squish top staff with every staff below it, returning mutated inputs
    squishOneIntoRest :: Staff -> [Staff] -> (Staff, [Staff])
    squishOneIntoRest top [] = (top, [])
    squishOneIntoRest top (bottom : bottoms) =
      let (top', bottom') = tryCombineStaves (top, bottom)
          (top'', bottoms') = squishOneIntoRest top' bottoms
       in (top'', bottom' : bottoms')

{- Greedy helper to try and combine two staves -}
tryCombineStaves :: (Staff, Staff) -> (Staff, Staff)
tryCombineStaves (a@(Staff s1s), b@(Staff s2s)) = case (s1s, s2s) of
  (m1 : m1s, m2 : m2s) ->
    join
      (mapStaffFromSingleSections $ tryCombineSections (m1, m2))
      (tryCombineStaves $ mapStaff (m1s, m2s))
  _ -> (a, b)
  where
    mapStaffFromSingleSections = mapPair (Staff . (: []))
    mapStaff = mapPair Staff

{- Greedy helper to try and combine two sections -}
tryCombineSections :: (Section, Section) -> (Section, Section)
tryCombineSections (s1, s2) =
  let (a, b) = mapMeasures (s1, s2)
   in case (a, b) of
        (m1 : m1s, m2 : m2s) ->
          mapSection $
            join
              (mapList $ tryCombineMeasures (m1, m2))
              (mapMeasures $ tryCombineSections $ mapSection (m1s, m2s))
        _ -> (s1, s2)
  where
    mapList = mapPair (: [])
    sec1 = Section (clef s1) (LP.key s1) (LP.time s1)
    sec2 = Section (clef s2) (LP.key s2) (LP.time s2)
    mapSection (a, b) = (sec1 a, sec2 b)
    mapMeasures = mapPair LP.measures

{- Greedy helper to try and combine two measures -}
tryCombineMeasures :: (LMeasure, LMeasure) -> (LMeasure, LMeasure)
tryCombineMeasures (top, bot) = combine (top, 0) (bot, 0)
  where
    combine :: (LMeasure, Dur) -> (LMeasure, Dur) -> (LMeasure, LMeasure)
    combine (LMeasure (a : as), n1) (LMeasure (b : bs), n2) =
      join (a', b') $ combine (as', n1 + measureDur a') (bs', n2 + measureDur b')
      where
        (dur1, dur2) = (elementDur a, elementDur b)
        ((a', b'), (as', bs')) = case (a, b) of
          (LRest {}, LNote {}) -> case compare n1 n2 of
            EQ -> case compare dur1 dur2 of
              LT -> tup (lm [a], lm [b]) (lm as, lm bs)
              EQ -> tup (lm [b], lm [a]) (lm as, lm bs)
              GT ->
                tup
                  (lm [b], lm [LRest dur2])
                  (lm $ LRest (dur1 - dur2) : as, lm bs)
            LT -> case compare dur1 dur2 of
              GT -> case compare (n1 + dur1) (n2 + dur2) of
                EQ ->
                  tup
                    (lm [LRest (n2 - n1), b], lm [LRest dur2])
                    (lm as, lm bs)
                GT ->
                  tup
                    (lm [LRest (n2 - n1), b], lm [LRest dur2])
                    (lm $ LRest ((n1 + dur1) - (n2 + dur2)) : as, lm bs)
                LT -> tup (lm [a], mempty) (lm as, lm (b : bs))
              _ -> tup (lm [a], mempty) (lm as, lm (b : bs))
            GT -> tup (mempty, lm [b]) (lm (a : as), lm bs)
          _ -> case compare n1 n2 of
            LT -> tup (lm [a], mempty) (lm as, lm (b : bs))
            _ -> tup (mempty, lm [b]) (lm (a : as), lm bs)
    combine (la, _) (lb, _) = (la, lb)

    tup = (,)
    lm = LMeasure
    joinCombine x y z = join x $ combine y z

{- Useful join tuples function for Monoid Semigroups for squishing staves -}
join :: (Monoid a, Semigroup a) => (a, a) -> (a, a) -> (a, a)
join (a, b) (a', b') = (a <> a', b <> b')

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
