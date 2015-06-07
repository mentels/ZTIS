module Demographic where
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.String.Utils as U
import qualified Data.Map.Strict as M
import qualified Data.Char as C
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

data DemographicData = DemographicData{countyName :: !String, population :: !Int, populationPerKm :: !Int,
                                        marriages :: !Double, -- per 1000
                                        divorces :: !Double,
                                        liveBirths :: !Double,
                                        totalDeaths :: !Double,
                                        naturalIncrease :: !Double,
                                        totalNetMigration :: !Double,
                                        annualGrowth :: !Double,
                                        infantsDeathPer1000Live :: !Double
                                       } deriving (Show)

instance Csv.FromRecord DemographicData where
    parseRecord v
        | V.length v == 11 = DemographicData <$> v Csv..! 0 <*> v Csv..! 1 <*> v Csv..! 2 <*> v Csv..! 3 <*> v Csv..! 4
                                             <*> v Csv..! 5 <*> v Csv..! 6 <*> v Csv..! 7 <*> v Csv..! 8 <*> v Csv..! 9
                                             <*> v Csv..! 10
        | otherwise     = mzero


readDemographicData :: IO (M.Map String DemographicData)
readDemographicData = do
    csvData <- BL.readFile "resources/demografia_wg_powiatow.csv"
    case Csv.decode Csv.HasHeader csvData of
        Left err -> return M.empty
        Right v ->  return $ V.foldl (\acc r -> M.insert (correctName $ countyName r) r acc) M.empty v

correctName :: String -> String
correctName = firstToUpper . U.strip

firstToUpper :: String -> String
firstToUpper (head:tail) = C.toUpper head : tail
firstToUpper [] = []