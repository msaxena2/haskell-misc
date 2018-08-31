import Data.Foldable
import Data.Monoid

-- Catamorphisms, or collapsing structure into values.

data MyData a = MyData a deriving Show

instance Foldable MyData where
  foldMap m t = case t of
                  (MyData d) -> m d

instance Functor MyData where
  fmap f (MyData a) = MyData $ f a

instance Applicative MyData where
  pure = MyData
  (<*>) (MyData f) (MyData c) = MyData $ f c

data Identity a = Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldMap f (Identity x) = f x


data Optional x = Yupp x
                | Nada
                deriving (Eq, Show)

instance Foldable Optional where
  foldMap f (Yupp x) = f x
  foldMap _ (Nada)   = mempty
