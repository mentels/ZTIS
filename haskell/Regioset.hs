{-# LANGUAGE ScopedTypeVariables #-}

import Utils
import Data.Char
import qualified Data.List.Utils as DLU
import Data.Maybe
import qualified Data.List as DL
import qualified Data.Set as S
import Text.HTML.TagSoup
import Network.HTTP.Conduit
import Data.Functor
import qualified Data.SetMap as SM
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map.Strict as M
import qualified Data.String.Utils as UT
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Monad
import Control.Applicative


data Councillor = Councillor {fullName :: String, year :: Int} deriving (Show, Read)
data County = County {countyName :: String, neighbors :: [String], councillors :: [Councillor] } deriving (Show, Read)
data Province = Province {provinceName :: String, counties:: [County]} deriving (Show, Read)

class PythonPrint a where
    pythonPrint :: a -> String

instance PythonPrint Councillor where
    pythonPrint c = "( " ++ printPythonString (fullName c) ++ ", " ++ show (year c) ++ " )"
instance PythonPrint County where
    pythonPrint c = "( " ++ printPythonString (countyName c) ++ ", " ++ printStringList (neighbors c) ++ " )"
instance PythonPrint Province where
    pythonPrint p = "{ \"provinceName\" : " ++ printPythonString (provinceName p) ++ ",\n \"counties\" : " ++ pythonPrint (counties p) ++ " }"
instance PythonPrint a => PythonPrint [a] where
    pythonPrint l = "[ " ++ DLU.join ",\n" (map pythonPrint l) ++ " ]"

printStringList :: [String] -> String
printStringList st = "[ " ++ DLU.join ", " (map printPythonString st)  ++ "]"
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
unemployedCsvFilePath = "bezrobotni_stopa_wg_powiatow_10_2014.csv"

main :: IO ()
main = writeUnemployedRateStats

data UnemployedRate = UnemployedRate { provinceCode :: !String, countyCode:: !String, name:: !String, unemployed :: !Double, unemployedRate :: !Double } deriving (Show)
instance Csv.FromRecord UnemployedRate where
    parseRecord v
        | V.length v == 5 = UnemployedRate <$>
                          v Csv..! 0 <*>
                          v Csv..! 1 <*>
                          v Csv..! 2 <*>
                          v Csv..! 3 <*>
                          v Csv..! 4
        | otherwise     = mzero


readUnemployedRate :: IO (M.Map String Double)
readUnemployedRate = do
    csvData <- BL.readFile unemployedCsvFilePath
    case Csv.decode Csv.NoHeader csvData of
        Left err -> return M.empty
        Right v -> let filtered = V.filter ( (/= "00") . countyCode) v
                       unemployedRateMap = V.foldl (\acc r -> M.insert (name r) (unemployedRate r) acc) M.empty filtered
                    in
                    return unemployedRateMap

data UnemployedRateStats = UnemployedRateStats {regionName :: !String, rate :: !Double, neighborsRate :: !Double, diff :: !Double} deriving (Show)
instance Csv.ToRecord UnemployedRateStats where
    toRecord (UnemployedRateStats regionName rate neighborsRate diff) = Csv.record [Csv.toField regionName, Csv.toField rate, Csv.toField neighborsRate, Csv.toField diff]

writeUnemployedRateStats :: IO ()
writeUnemployedRateStats = do
    rateMapping <- readUnemployedRate
    provinces <- load "regiosetProvincesCrawled.txt" :: IO [Province]
    let allCounties = concatMap counties provinces
    let stats = map (calculateCountyStats rateMapping) allCounties
    BLC.putStrLn $ Csv.encode stats

calculateCountyStats :: M.Map String Double -> County -> UnemployedRateStats
calculateCountyStats rateMapping county = UnemployedRateStats { regionName = countyName county, rate = r,
                                                                neighborsRate = nrnormalised, diff = r - nrnormalised} where
                                                                r = getOr0 (countyName county) rateMapping
                                                                nr = foldl (\acc nname -> acc + getOr0 nname rateMapping) 0 $ neighbors county
                                                                foundedNeighbors = length $ mapMaybe (`M.lookup` rateMapping) $ neighbors county
                                                                nrnormalised = nr / fromIntegral foundedNeighbors

getOr0 key map = fromMaybe 0 $ M.lookup key map


crawlNeighbors :: IO ()
crawlNeighbors = do
    tags <- parsePage encoding regiosetProvinces
    let provinceLinks = getProvinceLinks tags
    provincesPages <- parseRegiosetPages provinceLinks
    provinces <- mapM processProvincePage provincesPages
    putStrLn $ pythonPrint provinces
    save provinces "regiosetProvincesCrawled.txt"

-- returns suffixes of provinces urls
getProvinceLinks :: [Tag String] -> [String]
getProvinceLinks tags = mapMaybe extractUrl $ getFragment (containsText provincesStartText) (containsText provincesEndText) tags

processProvincePage :: [Tag String] -> IO Province
processProvincePage tags = do
    let countiesLinks = getCountiesLinks tags
    countiesPages <- parseRegiosetPages countiesLinks
    let countiesList = filter (not . DLU.startswith "Miasto" . countyName) $ map processCountyPage countiesPages
    return Province {provinceName = pn, counties = countiesList} where
        pn = UT.strip $ DLU.replace "Województwo " "" $ getHeaderText tags

getCountiesLinks :: [Tag String] -> [String]
getCountiesLinks tags = mapMaybe extractUrl $ getFragment (containsText "Powiaty w województwie") (containsText "Sąsiednie województwa") tags

processCountyPage :: [Tag String] -> County
processCountyPage tags = County {countyName = cn, neighbors = ns, councillors = []} where
                            cn = DLU.replace "Powiat " "" $ getHeaderText tags
                            fragment = getFragment (containsText "Sąsiednie powiaty") (containsText "Samorządowe Kolegium") tags
                            ns = filter (not . DL.isInfixOf "sąsiaduje") $ filter (isAlpha . head) $ map UT.strip $ mapMaybe maybeTagText fragment

getHeaderText :: [Tag String] -> String
getHeaderText tags = innerText $ getFragment (~== "<h1>") (~== "</h1>") tags

parseRegiosetPages :: [String] -> IO [[Tag String]]
parseRegiosetPages = mapM (parsePage encoding . (regiosetUrlPrefix ++))


