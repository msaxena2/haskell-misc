{-# LANGUAGE OverloadedStrings #-}

import Data.Text

-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative


data LoginError =   InvalidEmail
                  | NoSuchUser
                  | InvalidPassword
                   deriving Show


getDomain :: Text -> Either LoginError Text
getDomain email =
    case splitOn "@" email of
        [name, domain] ->
            Right domain
        _ ->
            Left InvalidEmail



printResult' :: Either LoginError Text -> IO ()
printResult' domain =
    case domain of
        Right text ->
            T.putStrLn (append "Domain: " text)
        Left InvalidEmail ->
            T.putStrLn "ERROR: Invalid domain"

getToken :: EitherIO LoginError Text
getToken = do
  liftIO (T.putStrLn "Enter email address:")
  input <- liftIO T.getLine
  liftEither (getDomain input)

liftEither :: Either e a -> EitherIO e a
liftEither x =
    EitherIO (return x)

liftIO :: IO a -> EitherIO e a
liftIO x =
    EitherIO (fmap Right x)


main = do
  putStrLn "Enter domain:"
  y <- T.getLine
  either  (const $ T.putStrLn "Invalid")
          (\b -> T.putStrLn (append "Domain" b)) $ getDomain $ y

users :: Map Text Text
users = Map.fromList
    [ ("example.com", "qwerty123")
    , ("localhost", "password")
    ]



getPasswordForToken :: Text -> EitherIO LoginError Text
getPasswordForToken token =
  let maybePassword = Map.lookup token users
   in maybe (liftEither $ Left NoSuchUser)
              (\x -> liftEither $ Right x)
              maybePassword


userLogin :: EitherIO LoginError Text

-- Ask the user for their email, and password, and
-- authenticate the email address.
userLogin = do
  token    <- getToken
  userPwd  <- getPasswordForToken token
  password <- liftIO (putStrLn "Enter Password" >> T.getLine)
  if (password == userPwd) then return token
                           else throwE InvalidPassword

throwE :: LoginError -> EitherIO LoginError Text
throwE e = liftEither $ Left e

userLogger :: IO ()
userLogger = do
  x <- runEitherIO userLogin
  case x of
    (Left err)  -> putStrLn $ show err
    (Right msg) -> T.putStrLn msg

lookupUser :: Text -> Maybe LoginError
lookupUser user =
  case (Map.lookup user users) of
    (Just password) -> Nothing
    Nothing         -> Just NoSuchUser


newtype EitherIO e a =
  EitherIO { runEitherIO :: IO (Either e a) }

instance Functor (EitherIO e) where
  fmap f (EitherIO eioa) = EitherIO $ (fmap.fmap) f eioa

instance Applicative (EitherIO e) where
  pure a = EitherIO $ (pure . pure) $ a
  (EitherIO eioab) <*> (EitherIO eioa) = EitherIO $
    (<*>) <$> eioab <*> eioa

instance Monad (EitherIO e) where
  -- (EitherIO e a) -> (a -> EitherIO e b) -> EitherIO e b
  (EitherIO eioa) >>= f = EitherIO $
                            do
                              x <- eioa
                              case x of
                                --err@(Left _) -> return err
                                (Left err) -> return $ Left err
                                (Right val)  -> runEitherIO (f val)

