import Network.CGI
import System.IO
import qualified System.IO.Strict as SIO

magicProjectList = "/tmp/projectList.txt"

class JSON a where
  jsonify :: a -> String
  jsonify a = "{}"

type Name = String
type Discription = String
type URL = String

type Project = (Name, Discription, URL)
newtype ProjType = ProjectType (Name, Discription, URL) deriving (Read, Show)

data Event = RegUser | GetTasks deriving (Show, Read)
data Returns = UserID Int | ProjectList [Project] | Nout deriving (Show, Read)

instance JSON Returns where
  jsonify (UserID id) = "\"UserID\" : \"" ++ show id ++ "\""
  jsonify (ProjectList ps) = concat ["{\"projects\" : [", commaConcat $ map (jsonify . ProjectType) ps, "] }"]

instance JSON ProjType where
  jsonify (ProjectType (n,d,u)) = concat ["{\"name\" : ", show n, ",\"desc\" : ", show d, ", \"url\" : ", show u, "}"]

commaConcat :: [String] -> String
commaConcat = foldr1 (\x xs -> x ++ ", " ++ xs)

getReqType :: (MonadCGI m) => m (Maybe Event)
getReqType =  readInput "action"

getProjects :: IO [Project]
getProjects = SIO.readFile magicProjectList >>= return . lines >>= return . map read

act :: Event -> IO Returns
act RegUser  = return (UserID 10)
act GetTasks = getProjects >>= return . ProjectList

act' :: Maybe Event -> IO Returns
act' = clean . fmap act
  where clean Nothing = return  Nout
        clean (Just a)  = a

cgiMain :: CGI CGIResult
cgiMain = getReqType >>= liftIO . act' >>= return . jsonify >>= (\o -> setHeader "Content-type" "application/json" >>= \_ -> return o) >>= output

main :: IO ()
main = runCGI (handleErrors cgiMain)
