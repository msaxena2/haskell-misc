-- Traversable needs sequenceA to be implemented.
-- sequenceA :: (Traversable t, Applicative f) => t ( f a) -> f (t a)
-- Broadly, flipping the algebraic structures


data MyData a = MyData a
  deriving (Eq, Show)

instance Functor MyData where
  fmap f (MyData x) = MyData $ f x

instance Foldable MyData where
  foldMap f (MyData x) = f x

