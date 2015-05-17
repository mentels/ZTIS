module Utils where

import qualified Data.Text as T
import Data.Char
import Data.List.Utils
import Data.Maybe
import Data.List
import qualified Data.Set as S
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import qualified Data.SetMap as SM
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map.Lazy as M
import qualified Data.Text.ICU.Convert as C

--TagSoup Utils

classIs :: String -> Tag String -> Bool
classIs cls (TagOpen _ attrs) = maybe False (isInfixOf cls) (lookup "class" attrs)
classIs _ _ = False

extractUrl :: Tag String -> Maybe String
extractUrl (TagOpen "a" attrs) = lookup "href" attrs
extractUrl _ = Nothing

containsText :: String -> Tag String -> Bool
containsText t tag = case tag of
                        (TagText txt) -> t `isInfixOf` txt
                        _ -> False

getFragment :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
getFragment from to tags = takeWhile (not . to) $ drop 1 $ dropWhile (not . from) tags


parsePage :: String -> String -> IO [Tag String]
parsePage encoding addr = do
    pageString <- loadPage encoding addr
    return $ parseTags pageString

loadPage :: String -> String -> IO String
loadPage encoding addr = do
     converter <- C.open encoding Nothing
     pageBytes <- simpleHttp addr
     return $ T.unpack $ C.toUnicode converter $ L.toStrict pageBytes

printPageTags :: String -> String -> IO ()
printPageTags encoding addr = do
    tags <- parsePage encoding addr
    print tags

printPage :: String -> String -> IO ()
printPage encoding addr = do
    page <- loadPage encoding addr
    print page


-- Graph Utils
type Adj = (String, [String])
type AdjList = [Adj]


nodesSizes :: [(a, [b])] -> [(a, Int)]
nodesSizes = map (\(p, n) ->(p, length n))

mirrorEdges :: AdjList -> AdjList
mirrorEdges l = M.assocs $ M.map S.toList $ SM.toMap $ foldr putMirroredEdges SM.empty l

putMirroredEdges :: Adj -> SM.SetMap String String -> SM.SetMap String String
putMirroredEdges (k, l) m = foldr (\node acc -> SM.insert node k $ SM.insert k node acc) m l


-- File Utils
load :: (Read a) => FilePath -> IO a
load f = do s <- readFile f
            return (read s)

save :: (Show a) => a -> FilePath -> IO ()
save x f = writeFile f (show x)



