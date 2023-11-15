{-# LANGUAGE OverloadedStrings #-}

module HScoreConfiguration where

import Data.Text
import Data.Yaml
import Euterpea (Dur, Mode (Major), PitchClass (C))
import MusicTypes

-- Default configuration (C Major, 4/4)
defaultTimeKeySpec :: TimeKeySpec
defaultTimeKeySpec = TimeKeySpec (KeySig C Major) (TimeSig 4 4) Nothing

defaultConfig :: MusicSpecs
defaultConfig = MusicSpecs [] True Nothing Nothing Nothing

{- Definitions for music specification-}
data MusicSpecs = MusicSpecs
  { timeKeySpecs :: [TimeKeySpec],
    autoAdjustAccidental :: Bool,
    pickUpDur :: Maybe Dur,
    midiSpec :: Maybe MidiSpec,
    title :: Maybe String
  }
  deriving (Eq, Read)

instance Show MusicSpecs where
  show (MusicSpecs tks acc pickup midi title) =
    showMaybe "Title: " title
      <> showMaybe "Midi: " midi
      <> showMaybe "Pickup: " pickup
      <> "Adjusting accidentals: "
      <> nl acc
      <> sections
    where
      showMaybe prefix m = case m of
        Nothing -> []
        (Just m') -> prefix <> nl m'
      sections = case tks of
        [] -> show defaultTimeKeySpec
        s -> Prelude.foldr1 (<>) (Prelude.map nl s)
      nl a = show a <> "\n"

newtype MidiSpec = Tempo Int
  deriving (Eq, Read)

instance Show MidiSpec where
  show (Tempo bpm) = show bpm <> "bpm"

data TimeKeySpec = TimeKeySpec
  { key :: KeySig,
    time :: TimeSig,
    measures :: Maybe Integer
  }
  deriving (Eq, Read)

instance Show TimeKeySpec where
  show (TimeKeySpec (KeySig pitch mode) (TimeSig num den) m) = case m of
    (Just meas) ->
      show pitch <+> show mode <+> show num <> "/" <> show den
        <+> "for"
        <+> show meas
        <+> "measures"
    Nothing -> show pitch <+> show mode <+> show num <> "/" <> show den
    where
      a <+> b = a <> " " <> b

{- Helper function to parse a Maybe Json Value -}
parseMaybe' :: FromJSON a => Maybe Value -> a -> Parser a
parseMaybe' Nothing a = pure a
parseMaybe' (Just v) a = parseJSON v

{- FromJSON Instances to allow YAML parsing -}
instance FromJSON MusicSpecs where
  parseJSON = withObject "MusicSpecs" $ \v -> do
    adjust <- v .:? "adjustAccidentals" :: Parser (Maybe Value)
    sections <- v .:? "sections" :: Parser (Maybe Value)
    pickup <- v .:? "pickup" :: Parser (Maybe Value)
    midi <- v .:? "midi" :: Parser (Maybe Value)
    title <- v .:? "title" :: Parser (Maybe Value)
    MusicSpecs
      <$> parseMaybe' sections []
      <*> parseMaybe' adjust True
      <*> parseMaybe' pickup Nothing
      <*> parseMaybe' midi Nothing
      <*> parseMaybe' title Nothing

instance FromJSON MidiSpec where
  parseJSON = withObject "MidiSpec" $ \v -> do
    tempo <- v .:? "tempo" :: Parser (Maybe Value)
    Tempo <$> parseMaybe' tempo 60

instance FromJSON TimeKeySpec where
  parseJSON = withObject "TimeKeySpec" $ \v -> do
    m <- v .:? "measures" :: Parser (Maybe Value)
    TimeKeySpec <$> v .: "key" <*> v .: "time" <*> parseMaybe' m Nothing

instance FromJSON KeySig where
  parseJSON = withObject "KeySig" $
    \v -> KeySig <$> v .: "keyPitch" <*> v .: "keyMode"

instance FromJSON PitchClass where
  parseJSON = withText "PitchClass" (return . read . unpack)

instance FromJSON Mode where
  parseJSON = withText "Mode" (return . read . unpack)

instance FromJSON TimeSig where
  parseJSON = withObject "TimeSig" $
    \v -> TimeSig <$> v .: "numerator" <*> v .: "denominator"