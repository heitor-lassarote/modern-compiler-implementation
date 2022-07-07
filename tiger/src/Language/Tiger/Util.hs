module Language.Tiger.Util
  ( findCycles
  , findDuplicatesOn
  , mapAccumLM
  ) where

import Control.Arrow ((&&&))
import Data.Bool (bool)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Traversable (for)
import Control.Monad.State (State, StateT (..), evalState, get, gets, modify, put)
import Data.Coerce (coerce)

data Vis = Visiting | Visited

-- | Find cycles in some graph. This is an implementation of
-- https://www.baeldung.com/cs/detecting-cycles-in-directed-graph#pseudocode
-- which states to be O(|V|+|E|), but I believe it is O((|V|+|E|)²) in the worst case.
findCycles :: forall a. (Eq a, Hashable a) => [(a, a)] -> [NonEmpty a]
findCycles xs =
  concat $ flip evalState HashMap.empty $
    for (HashMap.keys graph) \v ->
      gets (HashMap.member v) >>= bool
        (modify (HashMap.insert v Visiting) *> proccessDfsTree (v :| []))
        (pure [])
  where
    proccessDfsTree :: NonEmpty a -> State (HashMap a Vis) [NonEmpty a]
    proccessDfsTree stack@(top :| _) = do
      stacks <- for (HashSet.toList $ HashMap.findWithDefault HashSet.empty top graph) \v -> do
        visited <- get
        case HashMap.lookup v visited of
          Nothing       -> pure []
          Just Visiting -> pure [NE.reverse stack]
          Just Visited  -> put (HashMap.insert v Visiting visited) *> proccessDfsTree (v NE.<| stack)
      concat stacks <$ modify (HashMap.insert top Visited)

    graph :: HashMap a (HashSet a)
    graph
      = foldr (HashMap.unionWith (<>)) (HashMap.fromList $ ((, HashSet.empty) . snd) <$> xs)
      $ fmap (\(x, y) -> HashMap.singleton x $ HashSet.singleton y) xs

findDuplicatesOn :: Ord b => (a -> b) -> (a -> c) -> [a] -> [(b, NonEmpty c)]
findDuplicatesOn f g = fmap (f . NE.head &&& fmap g) . filter atLeast2 . NE.groupAllWith f
  where
    atLeast2 :: NonEmpty a -> Bool
    atLeast2 (_ :| _ : _) = True
    atLeast2 _            = False

mapAccumLM
  :: forall t s a b m
   . (Monad m, Traversable t)
  => (s -> a -> m (b, s))
  -> s
  -> t a
  -> m (t b, s)
mapAccumLM f acc ts = coerce (traverse @t @(StateT s m) @a @b) (flip f) ts acc
