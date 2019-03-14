-- This is a library for building projects. The idea is that this code will handle all IO and state 
-- so that each project simply provides a transformation from state to state dependne on event.
module ProjectM (
      Event (..),
      runSite,
      Result
) where

{-# LANGUAGE OverloadedStrings #-}
import Network.CGI
import System.IO
import qualified System.IO.Strict as SIO
import Database.Redis
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.IO.Unsafe
import Text.Read (readMaybe)

([>>=]) :: (Monad m1, Monad m2) => m1 (m2 a) -> (a -> m2 b) -> m1 (m2 b)
m [>>=] f = fmap (>>= f) m
infixl 1 [>>=]

data Event a = RequestJS | RequestInput | ReturnAns Int a | ShowProject deriving (Read, Show)
data Result = Send String | Yeet deriving (Show)

type Input = (String, String)
type Updater a b = a -> Event b -> Maybe (a, Result)

findInput :: [Input] -> String -> Maybe String
findInput xs s = (safeHead $ filter (\(a,_) -> a == s) xs) >>= return . snd
  where safeHead []     = Nothing
        safeHead (x:xs) = Just x

findInputM :: (Monad m) => [Input] -> String -> m (Maybe String)
findInputM ins = return . findInput ins

parseEvent :: (Read a) => Maybe String -> Maybe (Event a)
parseEvent s = s >>= readMaybe

runEvent :: (Monad m) => Updater a b -> a -> Maybe (Event b) -> m (Maybe (a, Result))
runEvent u i me = return (me >>= u i)

getResult :: (Monad m) => Maybe (a, Result) -> m Result
getResult (Just (_,r)) = return r
getResult Nothing      = return Yeet

cgiMain :: (Read b) => Updater a b -> a -> CGI CGIResult
cgiMain u init = getInputs >>= flip findInputM "event" >>= return . parseEvent >>= runEvent u init >>= getResult >>= return . show >>= output

runSite :: (Read a, Read b, Show a) => Updater a b -> a -> IO ()
runSite update init = runCGI (handleErrors $ cgiMain update init)
