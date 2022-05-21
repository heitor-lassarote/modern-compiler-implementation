module Language.Tiger.Util
  ( mapAccumLM
  ) where

import Control.Monad.State (StateT (..))
import Data.Coerce (coerce)

mapAccumLM
  :: forall t s a b m
   . (Monad m, Traversable t)
  => (s -> a -> m (b, s))
  -> s
  -> t a
  -> m (t b, s)
mapAccumLM f acc ts = coerce (traverse @t @(StateT s m) @a @b) (flip f) ts acc
