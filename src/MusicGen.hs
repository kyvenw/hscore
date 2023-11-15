-- http://lilypond.org/doc/v2.18/Documentation/notation/creating-midi-files
module MusicGen where

import Euterpea.Music
import EuterpeaTypes
import MusicTypes
import Test.QuickCheck

instance Arbitrary PitchClass where
  arbitrary =
    oneof $
      return
        <$> [ Cff,
              Cf,
              C,
              Dff,
              Cs,
              Df,
              Css,
              D,
              Eff,
              Ds,
              Ef,
              Fff,
              Dss,
              E,
              Ff,
              Es,
              F,
              Gff,
              Ess,
              Fs,
              Gf,
              Fss,
              G,
              Aff,
              Gs,
              Af,
              Gss,
              A,
              Bff,
              As,
              Bf,
              Ass,
              B,
              Bs,
              Bss
            ]
  shrink _ = []

maxLength = 3

genMusic :: Int -> Gen EMusic
genMusic 1 = Prim <$> arbitrary
genMusic n = oneof [(:+:) <$> genMusic (n -1) <*> genMusic (n -1)]

instance Arbitrary EMusic where
  arbitrary = resize maxLength $ sized genMusic
  shrink m = case m of
    (m1 :+: m2) -> [m1, m2]
    (m1 :=: m2) -> [m1, m2]
    _ -> []

instance Arbitrary EPrimitive where
  arbitrary =
    frequency
      [ (3, Note <$> genDur <*> genPitch),
        (1, Rest <$> genDur)
      ]
    where
      genPitch = (,) <$> arbitrary <*> elements [1 .. 10]
      genDur = elements [1, 1 / 2, 1 / 4]
  shrink _ = []
