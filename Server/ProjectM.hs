{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- This is a library for building projects. The idea is that this code will handle all IO and state 
-- so that each project simply provides a transformation from state to state dependne on event.
module ProjectM (
      Event (..),
      runSite,
      Result (..),
      Updater
) where

import Network.CGI
import System.IO
import qualified System.IO.Strict as SIO
import Database.Redis
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.IO.Unsafe
import Text.Read (readMaybe)
import Control.DeepSeq

(|>>=|) :: (Monad m1, Monad m2) => m1 (m2 a) -> (a -> m2 b) -> m1 (m2 b)
m |>>=| f = fmap (>>= f) m
infixl 1 |>>=|

data Event a = RequestJS | RequestInput | ReturnAns Int a | ShowProject deriving (Read, Show)
data Result = Send String | Yeet
data User = User Integer String String Integer deriving (Read, Show)

instance Show Result where
  show (Send a) = a
  show Yeet     = "Yeet"

type Input = (String, String)
type Updater a b = a -> Event b -> (a, Result)
type DBName = String

findInput :: [Input] -> String -> Maybe String
findInput xs s = (safeHead $ filter (\(a,_) -> a == s) xs) >>= return . snd
  where safeHead []     = Nothing
        safeHead (x:xs) = Just x

findInputM :: (Monad m) => [Input] -> String -> m (Maybe String)
findInputM ins = return . findInput ins

blitResult :: (Show a) => DBName -> Maybe a -> Maybe (a, Result) -> IO Result
blitResult db _ (Just (s,r))   = connect defaultConnectInfo >>= (\conn -> runRedis conn (lpush (BC.pack db) [BC.pack $ show s])) >> return r
blitResult db (Just i) Nothing = connect defaultConnectInfo >>= (\conn -> runRedis conn (lpush (BC.pack db) [BC.pack $ show i])) >> return Yeet --reblit initial state i
blitResult _  Nothing Nothing  = return Yeet --never got initial state, do nothing

updateUserScore :: Maybe (Event a) -> IO (Maybe (Event a))
updateUserScore e@(Just (ReturnAns id _)) = getUser (fromIntegral id) >>= setUser . (fmap incrScore) |>>=| \a -> (deepseq a e)
updateUserScore e                         = return e

incrScore :: User -> User
incrScore (User id s1 s2 i) = User id s1 s2 (i+1)

setUser :: Maybe User -> IO (Maybe Status)
setUser (Just (User id fname sname score)) = connect defaultConnectInfo >>= (\conn -> 
    runRedis conn (set (B.concat ["user:",BC.pack $ show id]) (BC.concat ["User ", BC.pack $ show id , " ", BC.pack $ show fname, " ", BC.pack $ show sname, " ", BC.pack $ show score]))) >>= maybify
  where maybify (Left _ ) = return Nothing
        maybify (Right i) = return (Just i)
setUser Nothing = return Nothing

getUser :: Integer -> IO (Maybe User)
getUser id = connect defaultConnectInfo >>= (\conn -> runRedis conn $ do
    let key = B.concat ["user:", BC.pack $ show id]
    user <- get key
    liftIO $ return user) >>= maybify >>= return . fmap (read . BC.unpack)
  where maybify (Left _ ) = return Nothing
        maybify (Right i) = return i

cgiMain :: (Read b, Show a) => DBName -> Updater a b -> Maybe a -> CGI CGIResult
cgiMain db u init = getInputs >>= flip findInputM "event" |>>=| readMaybe >>= liftIO . updateUserScore |>>=| (\e -> (init >>= \i -> return (u i e))) >>= \s -> liftIO (blitResult db init s) >>= return . show >>= output

existsDB :: String -> IO (Bool)
existsDB db = connect defaultConnectInfo >>= (\conn -> runRedis conn (exists $ BC.pack db)) >>= normalize
    where normalize (Left _)  = return False
          normalize (Right i) = return i

getState :: (Read a, Show a) => String -> a -> IO (Maybe a)
getState db _ = connect defaultConnectInfo >>= (\conn -> runRedis conn (blpop [BC.pack db] 0)) >>= normalize |>>=| readMaybe . BC.unpack . snd
    where normalize (Left _)  = return Nothing
          normalize (Right i) = return i

setState :: (Show a) => String -> a -> IO (Maybe a)
setState db s = connect defaultConnectInfo >>= (\conn -> runRedis conn (lpush (BC.pack db) [BC.pack $ show s])) >> return (Just s)

branch :: (a -> b) -> (a -> b) -> Bool -> (a -> b)
branch f _ True  = f
branch _ f False = f

runSite :: (Read a, Read b, Show a, Show b) => DBName -> Updater a b -> a -> IO ()
runSite db update init = existsDB db >>= \p -> branch (getState db) (setState db) p init >>= \i ->  runCGI (handleErrors $ cgiMain db update i)
