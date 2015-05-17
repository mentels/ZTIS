import Utils
import Data.Char
import Data.List.Utils
import Data.Maybe
import qualified Data.List as DL
import qualified Data.Set as S
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import Data.Functor
import qualified Data.SetMap as SM
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map.Lazy as M
import qualified Data.String.Utils as UT



data Councillor = Councillor {fullName :: String, year :: Int} deriving (Show)
data County = County {countyName :: String, neighbors :: [String], councillors :: [Councillor] } deriving (Show)
data Province = Province {provinceName :: String, counties:: [County]} deriving (Show)

class PythonPrint a where
    pythonPrint :: a -> String

instance PythonPrint Councillor where
    pythonPrint c = "( " ++ printPythonString (fullName c) ++ ", " ++ show (year c) ++ " )"
instance PythonPrint County where
    pythonPrint c = "( " ++ printPythonString (countyName c) ++ ", " ++ printStringList (neighbors c) ++ " )"
instance PythonPrint Province where
    pythonPrint p = "{ \"provinceName\" : " ++ printPythonString (provinceName p) ++ ",\n \"counties\" : " ++ pythonPrint (counties p) ++ " }"
instance PythonPrint a => PythonPrint [a] where
    pythonPrint l = "[ " ++ join ",\n" (map pythonPrint l) ++ " ]"

printStringList :: [String] -> String
printStringList st = "[ " ++ join ", " (map printPythonString st)  ++ "]"
printPythonString :: String -> String
printPythonString s = "\"" ++ s ++ "\""



wikipedia = "http://pl.wikipedia.org"
regiosetProvinces = "http://www.regioset.pl/gazeta.php?choice=1090"
regiosetDolnoslaskie = "http://www.regioset.pl/gazeta.php?choice=1"
regiosetBoleslawiec = "http://www.regioset.pl/gazeta.php?choice=2"
regiosetUrlPrefix = "http://www.regioset.pl/"
graphFileName = "graph.txt"
provincesStartText = "po wyborach w listopadzie 2014 r."
provincesEndText = "Olgierd GEBLEWICZ"
encoding = "iso88592"

main :: IO ()
main = crawlNeighbors

crawlNeighbors :: IO ()
crawlNeighbors = do
    tags <- parsePage encoding regiosetProvinces
    let provinceLinks = getProvinceLinks tags
    provincesPages <- parseRegiosetPages provinceLinks
    provinces <- mapM processProvincePage provincesPages
    putStrLn $ pythonPrint provinces

-- returns suffixes of provinces urls
getProvinceLinks :: [Tag String] -> [String]
getProvinceLinks tags = mapMaybe extractUrl $ getFragment (containsText provincesStartText) (containsText provincesEndText) tags

processProvincePage :: [Tag String] -> IO Province
processProvincePage tags = do
    let countiesLinks = getCountiesLinks tags
    countiesPages <- parseRegiosetPages countiesLinks
    let countiesList = filter (not . startswith "Miasto" . countyName) $ map processCountyPage countiesPages
    return Province {provinceName = pn, counties = countiesList} where
        pn = UT.strip $ replace "Województwo " "" $ getHeaderText tags

getCountiesLinks :: [Tag String] -> [String]
getCountiesLinks tags = mapMaybe extractUrl $ getFragment (containsText "Powiaty w województwie") (containsText "Sąsiednie województwa") tags

processCountyPage :: [Tag String] -> County
processCountyPage tags = County {countyName = cn, neighbors = ns, councillors = []} where
                            cn = replace "Powiat " "" $ getHeaderText tags
                            fragment = getFragment (containsText "Sąsiednie powiaty") (containsText "Samorządowe Kolegium") tags
                            ns = filter (not . DL.isInfixOf "sąsiaduje") $ filter (isAlpha . head) $ map UT.strip $ mapMaybe maybeTagText fragment

getHeaderText :: [Tag String] -> String
getHeaderText tags = innerText $ getFragment (~== "<h1>") (~== "</h1>") tags

parseRegiosetPages :: [String] -> IO [[Tag String]]
parseRegiosetPages = mapM (parsePage encoding . (regiosetUrlPrefix ++))


