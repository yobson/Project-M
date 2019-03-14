{-# LANGUAGE OverloadedStrings #-}

import Network.CGI
import System.IO
import qualified System.IO.Strict as SIO
import Database.Redis
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.IO.Unsafe

magicProjectList = "/tmp/projectList.txt"

class JSON a where
  jsonify :: a -> String
  jsonify a = "{}"

type Name = String
type Discription = String
type URL = String
type Input = (String, String)

type Project = (Name, Discription, URL)
newtype ProjType = ProjectType (Name, Discription, URL) deriving (Read, Show)

data Event = RegUser | GetTasks | GetUser deriving (Show, Read)
data Returns = UserID Integer String String Integer | ProjectList [Project] | Nout deriving (Show, Read)

data User = User Integer String String Integer deriving (Read, Show)

-- Not gonna lie... This was a crazy cool idea to nest monadic binds. Might change notation later
([>>=]) :: (Monad m1, Monad m2) => m1 (m2 a) -> (a -> m2 b) -> m1 (m2 b)
m [>>=] f = fmap (>>= f) m
infixl 1 [>>=]

instance JSON Returns where
  jsonify (UserID id fstN sndN scr) = "{\"userID\" : " ++ show id ++ ",\"firstName\" : " ++ show fstN ++ ",\"lastName\" : " ++ show sndN ++ ",\"score\" : " ++ show scr ++ "}"
  jsonify (ProjectList ps)      = concat ["{\"projects\" : [", commaConcat $ map (jsonify . ProjectType) ps, "] }"]
  jsonify (Nout)                = "{}"

instance JSON ProjType where
  jsonify (ProjectType (n,d,u)) = concat ["{\"name\" : ", show n, ",\"desc\" : ", show d, ", \"url\" : ", show u, "}"]

commaConcat :: [String] -> String
commaConcat = foldr1 (\x xs -> x ++ ", " ++ xs)

getReqType :: (MonadCGI m) => m (Maybe Event)
getReqType =  readInput "action"

getProjects :: IO [Project]
getProjects = SIO.readFile magicProjectList >>= return . lines >>= return . map read

findInput :: [Input] -> String -> Maybe String
findInput xs s = (safeHead $ filter (\(a,_) -> a == s) xs) >>= return . snd
  where safeHead []     = Nothing
        safeHead (x:xs) = Just x

getNewUserID :: IO (Maybe Integer)
getNewUserID = connect defaultConnectInfo >>= (\conn -> runRedis conn $ do
    id <- incr "user:id"
    liftIO $ return id) >>= maybify
  where maybify (Left _ ) = return Nothing
        maybify (Right i) = return $ Just i

setUser :: Integer -> String -> String -> Integer -> IO ()
setUser id fname sname score = connect defaultConnectInfo >>= (\conn -> runRedis conn $ do
    set (B.concat ["user:",BC.pack $ show id]) (BC.concat ["User ", BC.pack $ show id , " ", BC.pack $ show fname, " ", BC.pack $ show sname, " ", BC.pack $ show score])) >> return ()

getUser :: Integer -> IO (Maybe User)
getUser id = connect defaultConnectInfo >>= (\conn -> runRedis conn $ do
    let key = B.concat ["user:", BC.pack $ show id]
    user <- get key
    liftIO $ return user) >>= maybify >>= return . fmap (read . BC.unpack)
  where maybify (Left _ ) = return Nothing
        maybify (Right i) = return i

maybeIO :: IO (Maybe a) -> Maybe (IO a)
maybeIO a = unsafePerformIO a >>= return . return


act :: [Input] -> Event -> Maybe (IO Returns)
act ins RegUser  = do
                    let get = findInput ins
                    fstN <- get "firstName"
                    sndN <- get "secondName"
                    ioID <- maybeIO getNewUserID
                    return $ ioID >>= \id -> setUser id fstN sndN 0 >> return (UserID id fstN sndN 0)
act _   GetTasks = Just $ getProjects >>= return . ProjectList
act ins GetUser = findInput ins "id" >>= maybeIO . getUser . read [>>=] return . isoUser
  where isoUser (User id f s score) = UserID id f s score


act' :: Maybe Event -> [Input] -> IO Returns
act' me ins = clean $ (me >>= act ins)
  where clean Nothing = return  Nout
        clean (Just a)  = a

cgiMain :: CGI CGIResult
cgiMain = getReqType >>= (\t -> getInputs >>= (\ins -> liftIO $ act' t ins)) >>= return . jsonify >>= (\o -> setHeader "Content-type" "application/json" >>= \_ -> return o) >>= output

main :: IO ()
main = runCGI (handleErrors cgiMain)
