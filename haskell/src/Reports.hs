{-# LANGUAGE DeriveGeneric #-}

import qualified Demographic as D
import qualified Regioset as R
import qualified Utils as U
import qualified Data.Map.Strict as M
import qualified Data.Csv as Csv
import qualified GHC.Generics as G
import qualified Data.ByteString.Lazy as BL
import qualified Graphics.Histogram as GH
import Data.List (repeat)
import Data.Maybe (maybe, mapMaybe)
import Data.Map (elems)
import Control.Applicative ((<$>), (<*>))

data CountyStats = CountyStats {name :: !String, rate :: !Double, neighborsRate :: !Double, diff :: !Double,
                                councillorsAge :: !Double, naturalIncrease :: !Double} deriving (Show, G.Generic)

instance Csv.ToRecord CountyStats

main = plotNaturalIncreaseDiffsHistogram

writeCountyStats = do
   demographicData <- D.readDemographicData
   unemployedRate <- R.readUnemployedRate
   provinces <- R.readProvinces
   let  allCounties = concatMap R.counties provinces in
       BL.putStr $ Csv.encode $ map (calculateCountyStats unemployedRate demographicData) allCounties

calculateCountyStats :: M.Map String Double -> M.Map String D.DemographicData -> R.County -> CountyStats
calculateCountyStats rateMapping demographicMapping county =
    CountyStats { name = R.countyName county,
               rate = r,
               neighborsRate = nrnormalised,
               diff = abs $ r - nrnormalised,
               councillorsAge = avgAge $ R.councillors county,
               naturalIncrease = ni} where
    r = U.getOr0 (R.countyName county) rateMapping
    nr = foldl (\acc nname -> acc + U.getOr0 nname rateMapping) 0 $ R.neighbors county
    foundedNeighbors = length $ mapMaybe (`M.lookup` rateMapping) $ R.neighbors county
    nrnormalised = nr / fromIntegral foundedNeighbors
    age councillor = 2014 - fromIntegral (R.year councillor) :: Double
    avgAge list = sum (map age list) / fromIntegral (length list)
    ni = maybe (-100) D.naturalIncrease $ M.lookup (R.countyName county) demographicMapping

allNeighboursPairs :: [R.Province] -> [(String, String)]
allNeighboursPairs provinces =
    let
        allCounties = concatMap R.counties provinces
        repeatName county = repeat $ R.countyName county
    in
        concatMap (\c -> zip (repeatName c) (R.neighbors c)) allCounties


plotNaturalIncreaseDiffsHistogram = do
    demographicData <- D.readDemographicData
    provinces <- R.readProvinces
    let allPairs = allNeighboursPairs provinces
        getNatInc county = D.naturalIncrease `fmap` M.lookup county demographicData
        diffs = mapMaybe (\(c1, c2) -> subtract <$> getNatInc c1 <*> getNatInc c2) allPairs
    R.plotHistogram diffs "Roznica przyrostu naturalnego pomiedzy sasiednimi powiatami" "Roznica przyrostu naturalnego"
                          "Liczba par powiatów" "naturalIncreaseDiffHistogram.png"

plotNaturlaIncreaseHistogram = do
    demographicData <- D.readDemographicData
    let ni = map D.naturalIncrease (elems demographicData)
    R.plotHistogram ni "Przyrost naturalny w powiatach" "Przyrost naturalny na 1000 mieszkancow" "liczba powiatow"
                        "naturalIncreaseHistogram.png"