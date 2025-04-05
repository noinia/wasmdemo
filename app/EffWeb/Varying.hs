module EffWeb.Varying
  ( Varying(..)


  , ShouldRecompute(..)
  , HasShouldRecompute(..)
  ) where

import           Data.Profunctor

--------------------------------------------------------------------------------

data Varying input a = Constant a
                     | Varying (input -> a)
                     -- | Memo    (input -> ShouldRecompute)
                     --           (input -> a)
                     deriving stock (Functor)


instance Applicative (Varying input) where
  pure = Constant
  (Constant f) <*> (Constant x) = Constant $ f x
  -- f :: a -> b
  -- k :: input -> a
  (Constant f) <*> (Varying k)  = Varying $ f . k
  -- ff :: input -> (a -> b)
  (Varying ff) <*> (Constant x) = Varying $ \input -> ff input x
  (Varying ff) <*> (Varying k)  = Varying $ \input -> ff input (k input)

instance Monad (Varying input) where
  (Constant x) >>= k = k x
  (Varying f)  >>= k = Varying $ \input -> case k (f input) of
                                             Constant y -> y
                                             Varying g  -> g input
    -- TODO: verify that this satisfies the monad laws

instance Profunctor Varying where
  dimap f g = \case
    Constant x -> Constant (g x)
    Varying  h -> Varying  (g . h . f)
  rmap = fmap

-- instance Foldable (Varying input) where
--   foldMap = foldMapDefault

-- instance Traversable (Varying input) where
--   -- :: (a -> f b)  -> f (Varying input b)
--   traverse f = \case
--     Constant x -> Constant <$> f x
--     Varying k  -> Varying <$> traverse f k

-- traverseWith :: Applicative f => input -> (a -> f b) -> Varying input a -> f b


--------------------------------------------------------------------------------


data ShouldRecompute = Recompute | Reuse
  deriving stock (Show,Eq)

class HasShouldRecompute input where
  shouldRecompute :: input
                  -- ^ old
                  -> input
                  -- ^ new
                  -> ShouldRecompute
  default shouldRecompute :: Eq input => input -> input -> ShouldRecompute
  shouldRecompute old new | old == new = Reuse
                          | otherwise  = Recompute
