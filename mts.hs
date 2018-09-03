{- LANGUAGE GeneralizedNewtypeDeriving -}
import Control.Monad.Trans.MaybeT


newtype Identity a = Identity { runIdentity :: a } deriving (Show)

-- Monad Transformers only add aditional Monadic structure to the underlying
-- type. Hence they're written always as newtypes.

newtype Compose f g a = Compose {getCompose :: f (g a)} deriving (Eq, Show)

-- Functor Composition

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ fmap (fmap f) fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  (<*>) (Compose fgab)  (Compose fga) = Compose $ (<*>) <$> fgab <*> fga

newtype IdentityT m a = IdentityT { runIdentityT :: m a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT m) = IdentityT $ f <$> m

instance (Applicative m) => Applicative (IdentityT m) where
  (<*>) (IdentityT mab) (IdentityT ma) = IdentityT $ mab <*> ma

instance (Monad m) => Monad (IdentityT m) where
  (>>=) (IdentityT im) f = IdentityT $ im >>= (runIdentityT . f)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor f) => Functor (MaybeT f) where
  fmap func (MaybeT ma) = MaybeT $ (fmap . fmap) func ma

instance (Applicative f) => Applicative (MaybeT f) where
  pure a = MaybeT $ pure $ Just a
  (<*>) (MaybeT mab) (MaybeT Gma) = MaybeT $ (<*>) <$> mab <*> ma

instance (Monad m) => Monad (MaybeT m) where
  (MaybeT ma) >>= f =  MaybeT $
    do
      x <- ma
      case x of
        Nothing -> return Nothing
        (Just x) -> (runMaybeT .f) x

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance (Applicative m) => Applicative (EitherT e m) where
  pure a = EitherT $ pure $ Right a
  (EitherT emab) <*> (EitherT ema) = EitherT $ (<*>) <$> emab <*> ema

instance (Monad m) => Monad (EitherT e m) where
  return = pure
  (EitherT ema) >>= f = EitherT $
    do
      x <- ema
      case x of
        (Left x) -> return $ Left x
        (Right x) -> runEitherT $ f x

-- ((->) r) (m a)
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma


instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ pure . pure $ a
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma


-- instance Monad ((->) r) where
-- f :: a -> ((->) a c)

--  ((->) a b) >>= (.)

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r ->
    do
      a <- rma r
      runReaderT (f a) r

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Monad m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
    do
      (a', s') <- sma s
      return (f a', s')



