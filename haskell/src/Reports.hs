{-# LANGUAGE DeriveGeneric #-}

import qualified Demographic as D
import qualified Regioset as R
import qualified Utils as U
import qualified Data.Map.Strict as M
import qualified Data.Csv as Csv
import qualified GHC.Generics as G
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (maybe, mapMaybe)

data CountyStats = CountyStats {name :: !String, rate :: !Double, neighborsRate :: !Double, diff :: !Double,
                                councillorsAge :: !Double, naturalIncrease :: !Double} deriving (Show, G.Generic)

instance Csv.ToRecord CountyStats

main = do
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