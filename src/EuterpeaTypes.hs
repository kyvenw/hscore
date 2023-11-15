module EuterpeaTypes where

import Euterpea.Music

type EMusic = Music Pitch

type EPrimitive = Primitive Pitch

-- Useful property functions
lengthEMusic :: EMusic -> Dur
lengthEMusic m = fun m
  where
    fun m = case m of
      (Modify _ m) -> fun m
      (Prim (Rest dur)) -> dur
      (Prim (Note dur _)) -> dur
      (m1 :+: m2) -> fun m1 + fun m2
      (m1 :=: m2) -> max (fun m1) (fun m2)