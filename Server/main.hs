{-# LANGUAGE OverloadedStrings #-}

import Network.CGI
import System.IO
import qualified System.IO.Strict as SIO
import Database.Redis
import qualified Data.ByteString as B
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

data Event = RegUser | GetTasks deriving (Show, Read)
data Returns = UserID Integer String String | ProjectList [Project] | Nout deriving (Show, Read)

instance JSON Returns where
  jsonify (UserID id fstN sndN) = "{\"userID\" : \"" ++ show id ++ "\",\"firstName\" : " ++ show fstN ++ ",\"lastName\" : " ++ show sndN ++ "}"
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
getNewUserID= connect defaultConnectInfo >>= (\conn -> runRedis conn $ do
    id <- incr "user:id"
    liftIO $ return id) >>= maybify
  where maybify (Left _ ) = return Nothing
        maybify (Right i) = return $ Just i

maybeIO :: IO (Maybe a) -> Maybe (IO a)
maybeIO a = unsafePerformIO a >>= return . return


act :: [Input] -> Event -> Maybe (IO Returns)
act ins RegUser  = do
                    let get = findInput ins
                    fstN <- get "firstName"
                    sndN <- get "secondName"
                    ioID <- maybeIO getNewUserID
                    return $ ioID >>= \id -> return (UserID id fstN sndN)
act _   GetTasks = Just $ getProjects >>= return . ProjectList

act' :: Maybe Event -> [Input] -> IO Returns
act' me ins = clean $ (me >>= act ins)
  where clean Nothing = return  Nout
        clean (Just a)  = a

cgiMain :: CGI CGIResult
cgiMain = getReqType >>= (\t -> getInputs >>= (\ins -> liftIO $ act' t ins)) >>= return . jsonify >>= (\o -> setHeader "Content-type" "application/json" >>= \_ -> return o) >>= output

main :: IO ()
main = runCGI (handleErrors cgiMain)
