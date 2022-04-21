module Language.Tiger.Position
  ( Pos (..)
  , Range (..)
  , (<->)
  ) where

data Pos = Pos
  { line   :: !Int
  , column :: !Int
  , offset :: !Int
  } deriving stock (Eq, Show)


data Range = Range
  { start :: Pos
  , end :: Pos
  } deriving stock (Eq, Show)

-- | Extends a range, using the start position of the first range and the end
-- position of the second range. Note that the precondition that the former must
-- start before the latter is not checked.
(<->) :: Range -> Range -> Range
Range p1Start _ <-> Range _ p2End = Range p1Start p2End
