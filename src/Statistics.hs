{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Monoid
import           Options.Applicative
import           Pipes
import qualified Pipes.ByteString    as P (fromHandle)
import           Pipes.Csv
import qualified Pipes.Prelude       as P hiding (fromHandle)
import           System.IO.Temp
import           System.Process

import           Conversion
import           Parsing
import           Types
import           Util

data StatsSettings = StatsSettings
  { outputTempUOM :: TemperatureUOM
  , outputLocUOM  :: LocationUOM
  , inputFile     :: FilePath
  }

parseStatsSettings :: Parser StatsSettings
parseStatsSettings = StatsSettings
    <$> option auto
        (   long "temp-uom"
         <> short 't'
         <> value Kelvin
         <> metavar "UOM"
         <> help "UOM to use for temperature output")
    <*> option auto
        (   long "dist-uom"
         <> short 'd'
         <> value Metre
         <> metavar "UOM"
         <> help "UOM to use for distance output")
    <*> strOption
        (   long "input-file"
         <> short 'f'
         <> metavar "INPUT_FILE"
         <> help "File to read data from")

-- statistics stored in metre-kelvin
data Statistics = Statistics
  { minTemp       :: Double
  , maxTemp       :: Double
  , avgTemp       :: Double
  , numObvs       :: Int
  , obvsPerObs    :: Map Observatory Int
  , distTravelled :: Double
  , firstPoint    :: (Double, Double)
  , lastPoint     :: (Double, Double)
  } deriving Show

displayStats :: Statistics -> TemperatureUOM -> LocationUOM -> IO ()
displayStats Statistics{..} tempUOM locUOM = do
    let [min', max', avg'] = map (rawConvertTemp Kelvin tempUOM) [minTemp, maxTemp, avgTemp]
    let dist  = rawConvertLen Metre locUOM distTravelled
    putStrLn $ "Min Temp: " <> show min' <> show tempUOM
    putStrLn $ "Max Temp: " <> show max' <> show tempUOM
    putStrLn $ "Avg Temp: " <> show avg' <> show tempUOM
    putStrLn $ "Distance Travelled: " <> show dist <> show locUOM
    forM_ (M.toList obvsPerObs) $ \(obs, count) ->
        putStrLn $ "Observations for " <> show obs <> ": " <> show count
    putStrLn $ "Total observations: " <> show numObvs

combineStats :: Statistics -> Statistics -> Statistics
combineStats (Statistics min1 max1 m1 n1 o1 dist1 fp1 lp1)
             (Statistics min2 max2 m2 n2 o2 dist2 fp2 lp2)
        = let min'  = min min1 min2
              max'  = max max1 max2
              n'    = n1 + n2
              o'    = M.unionWith (+) o1 o2
              m'    = (m1*fromIntegral n1 + m2*fromIntegral n2) / fromIntegral n'
              dist' = rawDistance lp1 fp2 + dist1 + dist2
            in Statistics min' max' m' n' o' dist' fp1 lp2

singleton :: StatsLine -> Statistics
singleton (StatsLine loc temp obs) =
    Statistics temp temp temp 1 (M.singleton obs 1) 0 loc loc

main :: IO ()
main = do
    StatsSettings{..} <- execParser (info parseStatsSettings fullDesc)
    withTempFile "." "tmpstats" $ \tmpFile h -> do
        system $ "sort " <> inputFile <> " > " <> tmpFile
        stats <- generateStats $
            decodeWith weatherDecodeOptions NoHeader (P.fromHandle h) >->
            filterRight >->
            P.map statsLine >->
            P.map singleton
        case stats of
            Left err -> putStrLn err
            Right stats' -> displayStats stats' outputTempUOM outputLocUOM
  where
    filterRight :: Pipe (Either String LogLine) LogLine IO ()
    filterRight = forever $ do
        x <- await
        case x of
            Left _   -> return ()
            Right x' -> yield x'
    generateStats source = do
        res <- next source
        case res of
            Left _ -> return $ Left "No valid lines"
            Right (initStat, statLines) -> do
                stats <- P.fold combineStats initStat id statLines
                return $ Right stats
