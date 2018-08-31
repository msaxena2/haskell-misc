data WhoCares a = ItDoesnt
                | Matter a
                | WhatThisIsCalled
                deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt         = WhatThisIsCalled
  fmap f (Matter a)       = Matter $ f a
  fmap _ WhatThisIsCalled = ItDoesnt

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

data BoolAndSomethingElse a = False' a
                            | True'
                            deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a)  = False' $ f a
  fmap f _ = True'

data Quant a b =
      Finance a
    | Desk b
    | Bloor b
    deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Finance x) = Finance x
  fmap f (Desk b)    = Desk $ f b
  fmap f (Bloor b)   = Bloor $ f b

data K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a


data EvilGoats a b = GoatsyConst b

instance Functor (EvilGoats a) where
  fmap f (GoatsyConst b) = GoatsyConst $ f b


data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

