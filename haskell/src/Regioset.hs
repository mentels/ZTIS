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
import qualified Graphics.Histogram as GH
import qualified System.Exit as SE
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as HistOpts
import qualified Text.Regex.TDFA as R

class PythonPrint a where
    pythonPrint :: a -> String

data Councillor = Councillor {fullName :: String, year :: Int} deriving (Show, Read)
data County = County {countyName :: String, neighbors :: [String], councillors :: [Councillor] } deriving (Show, Read)
data Province = Province {provinceName :: String, counties:: [County]} deriving (Show, Read)
data UnemployedRate = UnemployedRate { provinceCode :: !String, countyCode:: !String, name:: !String, unemployed :: !Double, unemployedRate :: !Double } deriving (Show)
data UnemployedRateStats = UnemployedRateStats {regionName :: !String, rate :: !Double, neighborsRate :: !Double, diff :: !Double} deriving (Show)


instance Csv.ToRecord UnemployedRateStats where
    toRecord (UnemployedRateStats regionName rate neighborsRate diff) = Csv.record [Csv.toField regionName, Csv.toField rate, Csv.toField neighborsRate, Csv.toField diff]
instance Csv.FromRecord UnemployedRate where
    parseRecord v
        | V.length v == 5 = UnemployedRate <$>
                          v Csv..! 0 <*>
                          v Csv..! 1 <*>
                          v Csv..! 2 <*>
                          v Csv..! 3 <*>
                          v Csv..! 4
        | otherwise     = mzero

instance PythonPrint Councillor where
    pythonPrint c = "( " ++ printPythonString (fullName c) ++ ", " ++ show (year c) ++ " )"
instance PythonPrint County where
    pythonPrint c = "( " ++ printPythonString (countyName c) ++ ", " ++ printStringList (neighbors c) ++ ", " ++ pythonPrint (councillors c) ++ " )"
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
radniPowiatuUrlSuffix = "&dr=rady"

-- main :: IO SE.ExitCode
-- main = writeUnemployedRateStats

-- main :: IO ()
main = writeCouncillorsStats

readUnemployedRate :: IO (M.Map String Double)
readUnemployedRate = do
    csvData <- BL.readFile unemployedCsvFilePath
    case Csv.decode Csv.NoHeader csvData of
        Left err -> return M.empty
        Right v -> let filtered = V.filter ( (/= "00") . countyCode) v
                       unemployedRateMap = V.foldl (\acc r -> M.insert (name r) (unemployedRate r) acc) M.empty filtered
                    in
                    return unemployedRateMap


writeUnemployedRateStats :: IO SE.ExitCode
writeUnemployedRateStats = do
    rateMapping <- readUnemployedRate
    provinces <- load "regiosetProvincesCrawled.txt" :: IO [Province]
    let allCounties = concatMap counties provinces
    let stats = map (calculateCountyStats rateMapping) allCounties
    plotUnemployedRatesNeihborsDiffHistogram stats

calculateCountyStats :: M.Map String Double -> County -> UnemployedRateStats
calculateCountyStats rateMapping county = UnemployedRateStats { regionName = countyName county, rate = r,
                                                                neighborsRate = nrnormalised, diff = r - nrnormalised} where
                                                                r = getOr0 (countyName county) rateMapping
                                                                nr = foldl (\acc nname -> acc + getOr0 nname rateMapping) 0 $ neighbors county
                                                                foundedNeighbors = length $ mapMaybe (`M.lookup` rateMapping) $ neighbors county
                                                                nrnormalised = nr / fromIntegral foundedNeighbors

plotUnemployedRatesNeihborsDiffHistogram :: [UnemployedRateStats] -> IO SE.ExitCode
plotUnemployedRatesNeihborsDiffHistogram stats =
    let diffs = map (abs . diff) stats
        hist = GH.histogram GH.binSturges diffs
        opts =  Opts.add (Opt.custom "terminal" "") ["png size 1024,768 enhanced font 'Verdana,10'"] $
                Opts.add (Opt.custom "encoding" "") ["utf8"] $
                Opts.title "Porownanie stop bezrobocia miedzy sasiednimi powiatami" $
                Opts.xLabel "Roznica pomiedzy stopa bezrobocia powiatu i srednia stopa bezrobocia jego sasiadow" $
                Opts.yLabel "Ilosc powiatow" $
                HistOpts.clusteredGap 0.0 $
                Opts.xRange2d (0, 18) $
                GH.defOpts hist
    in
    GH.plotAdv "unemployedNeighborsDiffHistogram.png" opts hist

writeCouncillorsStats :: IO SE.ExitCode
writeCouncillorsStats = do
    provinces <- load "regiosetProvincesCrawled.txt" :: IO [Province]
    let councillorsInCounties = map councillors $ concatMap counties provinces
        age councillor = 2014 - fromIntegral (year councillor) :: Double
        avgAge list = sum (map age list) / fromIntegral (length list)
        avgAgeInCounties = map avgAge councillorsInCounties
        allCouncillors = concat councillorsInCounties
    plotCouncillorsBirthYear allCouncillors
    plotAvgCountyCouncillorsAge avgAgeInCounties

plotCouncillorsBirthYear :: [Councillor] -> IO SE.ExitCode
plotCouncillorsBirthYear councillors =
    let years = map (fromIntegral . year) councillors
        hist = GH.histogram GH.binSturges years
        opts =  Opts.add (Opt.custom "terminal" "") ["png size 1024,768 enhanced font 'Verdana,10'"] $
                Opts.add (Opt.custom "encoding" "") ["utf8"] $
                Opts.title "Lata urodzin radnych powiatow" $
                Opts.xLabel "Rok urodzenia" $
                Opts.yLabel "Ilosc radnych" $
                HistOpts.clusteredGap 0.0 $
                Opts.xRange2d (0, 15) $
                GH.defOpts hist
    in
    GH.plotAdv "councillorsBirthYearHistogram.png" opts hist

plotAvgCountyCouncillorsAge :: [Double] -> IO SE.ExitCode
plotAvgCountyCouncillorsAge ages =
    let hist = GH.histogram GH.binSturges ages
        opts =  Opts.add (Opt.custom "terminal" "") ["png size 1024,768 enhanced font 'Verdana,10'"] $
                Opts.add (Opt.custom "encoding" "") ["utf8"] $
                Opts.title "Sredni wiek radnych w powiecie" $
                Opts.xLabel "Wiek" $
                Opts.yLabel "Ilosc powiatow" $
                HistOpts.clusteredGap 0.0 $
--                 Opts.xRange2d (0, 15) $
                GH.defOpts hist
    in
    GH.plotAdv "avgCountyCouncillorAgeHistogram.png" opts hist

getOr0 key map = fromMaybe 0 $ M.lookup key map


crawlAllProvinces :: IO ()
crawlAllProvinces = do
    tags <- parsePage encoding regiosetProvinces
    let provinceLinks = extractProvincesLInks tags
    provincesPages <- parseRegiosetPages provinceLinks
    provinces <- mapM readProvinceFromPage provincesPages
--     putStrLn $ pythonPrint provinces
    save provinces "regiosetProvincesCrawled.txt"
    writeFile "withCouncillors.py" (pythonPrint provinces)
    where
        -- returns suffixes of provinces urls
        extractProvincesLInks tags = mapMaybe extractUrl $ getFragment (containsText provincesStartText) (containsText provincesEndText) tags

readProvinceFromPage :: [Tag String] -> IO Province
readProvinceFromPage tags = do
    let countiesLinks = extractCountiesLinks tags
    countiesPages <- parseRegiosetPages countiesLinks
    let countiesPagesWithLinks = zip countiesLinks countiesPages
    countiesList <- mapM readCountyFromPage countiesPagesWithLinks
    let noCities = filter (not . DLU.startswith "Miasto" . countyName) countiesList
    return Province {provinceName = pn, counties = noCities} where
        pn = UT.strip $ DLU.replace "Województwo " "" $ extractHeader tags
        extractCountiesLinks tags = mapMaybe extractUrl $ getFragment (containsText "Powiaty w województwie") (containsText "Sąsiednie województwa") tags

readCountyFromPage :: (String, [Tag String]) -> IO County
readCountyFromPage (link, tags) = do
    [councillorsPage] <- parseRegiosetPages [link ++ radniPowiatuUrlSuffix]
    let councillorsText =  innerText $ getFragment (containsText "Radni") (~== "</div>") councillorsPage
        parsedCouncillors = councillorsText R.=~ "\r\n(.*) [(]ur[.] ([0-9]{4})" :: [[String]]
        councillorsList = map (\(_:name:birthYear:_) -> Councillor {fullName = name, year = read birthYear :: Int} ) parsedCouncillors
    print $ head councillorsList
    return County {countyName = cn, neighbors = ns, councillors = councillorsList} where
                            cn = DLU.replace "Powiat " "" $ extractHeader tags
                            fragment = getFragment (containsText "Sąsiednie powiaty") (containsText "Samorządowe Kolegium") tags
                            ns = filter (not . DL.isInfixOf "sąsiaduje") $ filter (isAlpha . head) $ map UT.strip $ mapMaybe maybeTagText fragment

extractHeader :: [Tag String] -> String
extractHeader tags = innerText $ getFragment (~== "<h1>") (~== "</h1>") tags

parseRegiosetPages :: [String] -> IO [[Tag String]]
parseRegiosetPages = mapM (parsePage encoding . (regiosetUrlPrefix ++))


