{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables, OverlappingInstances #-}
module Path.Evolver.BoundedEnum where

import Random

class (Bounded a, Enum a) => BoundedEnum a
instance (Bounded a, Enum a) => BoundedEnum a

instance BoundedEnum a => Random a where
    random g =
        let min = fromEnum (minBound :: a) in
        let max = fromEnum (maxBound :: a) in
        let (i, g') = randomR (min, max) $ g in
        (toEnum i, g')
    randomR (low, high) g =
        let min = fromEnum low in
        let max = fromEnum high in
        let (i, g') = randomR (min, max) $ g in
        (toEnum i, g')
