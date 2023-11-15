-- A PrettyPrinter that prints LilyPond representation into LilyPond code
module LilyPondPrinter (pretty, oneLine, formatDuration) where

import Data.Char (toLower)
import Data.Ratio
import Euterpea (Dur, Mode, Octave, Pitch, PitchClass)
import EuterpeaConverter
import EuterpeaTypes
import HScoreConfiguration
import LilyPondSyntax
import MusicTypes
import Text.PrettyPrint hiding (Mode, braces, parens, sep)
import qualified Text.PrettyPrint as PP
import Prelude hiding ((<>))

class PP a where
  pp :: a -> Doc

-- Top-level pretty-printer
pretty :: PP a => a -> String
pretty = PP.render . pp

-- One-liner pretty-printer
oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

ppMaybe :: PP a => Maybe a -> Doc
ppMaybe Nothing = mempty
ppMaybe (Just a) = pp a

-- Pads delimiter wrapping functions
spaced :: (Doc -> Doc) -> (Doc -> Doc)
spaced f d = f $ PP.space <> d <> PP.space

padBraces :: Doc -> Doc
padBraces = spaced PP.braces

mapPP :: PP a => [a] -> [Doc]
mapPP = map pp

block :: String -> Doc -> Doc
block s d = pp "\\" <> pp s <+> PP.braces d

ppn :: PP a => a -> Doc
ppn p = pp p <> pp "\n"

npp :: PP a => a -> Doc
npp p = pp "\n" <> pp p

pps :: Show a => a -> Doc
pps = pp . show

instance PP String where
  pp s = PP.text s

instance PP LilyPond where
  pp (LilyPond staves specs) = case (midiSpec specs, title specs) of
    (Just (Tempo bpm), Just name) ->
      book $ header name <+> score (pstaves <+> layout <+> midi bpm)
    (Just (Tempo bpm), Nothing) ->
      book $ header "" <+> score (pstaves <+> layout)
    (Nothing, Just name) -> book $ header name <+> score (pstaves <+> layout)
    (Nothing, Nothing) -> book $ header "" <+> score pstaves
    where
      angleBrackets p = ppn "<<" <+> p <+> ppn ">>"
      book p = version <> block "book" p
      layout = block "layout" mempty
      midi bpm = block "midi" (pp "\\tempo 4 =" <+> pps bpm)
      header title =
        block "header" $
          pp "title =" <+> PP.doubleQuotes (pp title)
            <+> pp "composer ="
            <+> PP.doubleQuotes (pp "Arranged by hscore")
      score p = block "score" p
      pstaves = angleBrackets . hsep $ mapPP staves
      version = ppn "\\version \"2.22.1\""

instance PP Staff where
  pp (Staff sections) = block "new Staff" $ hsep (mapPP sections) <> pp "\n"

instance PP Section where
  pp (Section c k t []) = pp c <+> pp k <+> pp t
  pp (Section c k t (m : ms)) =
    pp c <+> pp k <+> pp t <+> pp "\\partial"
      <+> ppMaybe (formatDuration (measureDur m))
      <+> hsep (mapPP (m : ms))

instance PP LMeasure where
  pp (LMeasure m) = hsep $ mapPP m

instance PP Clef where
  pp Treble = pp "\\clef \"treble\""
  pp Alto = pp "\\clef \"alto\""
  pp Tenor = pp "\\clef \"tenor\""
  pp Bass = pp "\\clef \"bass\""

instance PP KeySig where
  pp (KeySig keyPitch keyMode) = pp keyPitch <+> pp keyMode

instance PP PitchClass where
  pp pitch = pp "\\key" <+> pp (formatPitch pitch)

instance PP Mode where
  pp mode = pp "\\" <> pp (map toLower (show mode))

instance PP TimeSig where
  pp (TimeSig num denom) = pp "\\time" <+> pps num <> pp "/" <> pps denom

instance PP LRawStaff where
  pp (LRawStaff elts) = hsep $ mapPP elts

instance PP LElement where
  pp (LRest dur) = case formatDuration dur of
    Nothing ->
      let (newRest1, newRest2) = trySplit (LRest dur)
       in pp newRest1 <+> pp newRest2
    Just durString -> pp "r" <> pp durString
  pp (LNote dur tie p) = case formatDuration dur of
    Nothing ->
      let (newNote1, newNote2) = trySplit (LNote dur tie p)
       in pp newNote1 <+> pp newNote2
    Just durString ->
      if tie
        then pp p <> pp durString <> pp "~"
        else pp p <> pp durString

instance PP Pitch where
  pp (pc, o) = pp (formatPitch pc) <> pp (octaveMarks o)

octaveMarks :: Octave -> String
octaveMarks o = case compare o 3 of
  EQ -> ""
  GT -> replicate (o - 3) '\''
  LT -> replicate (3 - o) ','

formatPitch :: PitchClass -> String
formatPitch p = case map toLower (show p) of
  (x : "s") -> x : "is"
  (x : "f") -> x : "es"
  (x : "ss") -> x : "isis"
  (x : "ff") -> x : "eses"
  x -> x

formatDuration :: Dur -> Maybe String
formatDuration ratio =
  let (n, d) = (numerator ratio, denominator ratio)
   in case d `mod` n of
        0 -> Just $ show (d `div` n)
        _ ->
          if d `mod` (n - 1) /= 0
            then formatDuration ((n - 1) % d) >>= \dur -> return $ dur ++ "."
            else
              if 1 /= (n `div` 2)
                then Nothing
                else Just $ show (d `div` (n - 1)) ++ "."

trySplit :: LElement -> (LElement, LElement)
trySplit (LNote dur tie pitch) =
  let (n, d) = (numerator dur, denominator dur)
   in (LNote ((n - 1) % d) True pitch, LNote (1 % d) tie pitch)
trySplit (LRest dur) =
  let (n, d) = (numerator dur, denominator dur)
   in (LRest ((n - 1) % d), LRest (1 % d))