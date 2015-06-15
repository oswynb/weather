{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BSC
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           Options.Applicative
import           Pipes
import qualified Pipes.ByteString                as P (toHandle)
import           Pipes.Csv
import qualified Pipes.Prelude                   as P hiding (toHandle)
import           System.IO
import           System.Locale
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           Conversion
import           Parsing
import           Types
import           Util

-- | We roughly model the balloon system stepwise
--   TimeStamps   -> Some constant time step
--   Location     -> Basic brownian-like motion with a tendency
--   Temperature  -> Minor fluctuations each step
--   Observatory  -> Each step send to observatories within a set range
--   Data Quality -> Randomly have it such that an observatory logs poorly
--   We spread the values using a normal distribution
--   All values must be non-negative bar the location drift values and time
--   steps. Negative time steps can be used to simulate out-of-order data

data GenSettings = GenSettings
    { genStartTime    :: TimeStamp -- Initial TimeStamp for generated data
    , genTimeStep     :: Double    -- Average timestep in minutes
    , genLocStepX     :: Double    -- Average x-coord distance to travel per timestep
    , genLocStepY     :: Double    -- Average y-coord distance to travel per timestep
    , genTempStep     :: Double    -- Variance in temperature for each timestep
    , genBalloonRange :: Double    -- Range of balloon broadcast
    , genFailRate     :: Double    -- Failure rate at which to generate invalid data
    , genSystemSize   :: Int       -- Size of the system
    , genNumLines     :: Int       -- Number of lines to generate
    , genFile         :: FilePath  -- File to write output to
    }

parseGenSettings :: Parser GenSettings
parseGenSettings = GenSettings
    <$> option auto
        (   long "start-time"
         <> short 's'
         <> value defaultStartTime
         <> metavar "START_TIME"
         <> help ("Time to start generating data from in format: " <> tsFormat))
    <*> option auto
        (   long "time-step"
         <> short 't'
         <> value defaultTimeStep
         <> metavar "TIME_STEP"
         <> help "Average time step in minutes")
    <*> option auto
        (   long "x-drift"
         <> short 'x'
         <> value defaultLocXStep
         <> metavar "X_DRIFT"
         <> help "Average x-coord drift per time step in metres")
    <*> option auto
        (   long "y-drift"
         <> short 'y'
         <> value defaultLocYStep
         <> metavar "Y_DRIFT"
         <> help "Average y-coord drift per time step in metres")
    <*> (nonNegative "temp-variance" <$> option auto
        (   long "temp-variance"
         <> short 'p'
         <> value defaultTempStep
         <> metavar "TEMP_VARIANCE"
         <> help "Variance in temperature in kelvin"))
    <*> pure defaultBalloonRange
    <*> (nonNegative "fail-rate" <$> option auto
        (   long "fail-rate"
         <> short 'r'
         <> value defaultFailRate
         <> metavar "FAIL_RATE"
         <> help "Rate at which observatories produce rubbish output [0,1)"))
    <*> pure defaultSystemSize
    <*> option auto
        (   long "num-lines"
         <> short 'n'
         <> value defaultNumLines
         <> metavar "NUM_LINES"
         <> help "Number of lines to output")
    <*> strOption
        (   long "output-file"
         <> short 'f'
         <> value defaultOutputFile
         <> metavar "OUTPUT_FILE"
         <> help "File to output generated data to")

nonNegative :: String -> Double -> Double
nonNegative name x = if x >= 0 then x else error (name ++ " must be non-negative")

defaultStartTime :: TimeStamp
defaultTimeStep, defaultLocXStep, defaultLocYStep,
    defaultTempStep, defaultBalloonRange, defaultFailRate :: Double
defaultSystemSize, defaultNumLines :: Int
defaultOutputFile :: String

defaultStartTime    = TimeStamp $ fromJust $ parseTime defaultTimeLocale tsFormat "2014-06-08T10:30"
defaultTimeStep     = 15
defaultLocXStep     = 1200
defaultLocYStep     = -1800
defaultTempStep     = 0.1
defaultBalloonRange = 30000
defaultFailRate     = 0.08
defaultSystemSize   = 100000
defaultNumLines     = 100000
defaultOutputFile   = "gen-weather-sample.csv"

data ObservatoryOutput = Valid   LogLine
                       | Invalid ByteString

instance ToRecord ObservatoryOutput where
    toRecord (Valid ll)  = toRecord ll
    toRecord (Invalid x) = toRecord [x]

--   We use Metres and Kelvin for the System internally
data System = System
    { systemTime   :: TimeStamp
    , balloonLoc   :: Location
    , balloonRange :: Double
    , systemTemp   :: Temperature
    , systemObss   :: [(Observatory, Location)]
    , systemSize   :: Int
    }

type Mutator x = Gen (PrimState IO) -> x -> IO x

data Mutators = Mutators
    { mutTime :: Mutator TimeStamp
    , mutLoc  :: Mutator Location
    , mutTemp :: Mutator Temperature
    , mutLine :: Mutator ObservatoryOutput
    }

observatoryLocs :: [(Observatory, Location)]
observatoryLocs = [ (Observatory "AU", Location 10000 10000)
                  , (Observatory "FR", Location 80000 40000)
                  , (Observatory "US", Location 30000 50000)
                  , (Observatory "NZ", Location 10000 30000)
                  ]

initialise :: GenSettings -> (Mutators, System)
initialise GenSettings{..} =
    let mutTime g (TimeStamp x) = do
            v <- normal genTimeStep (genTimeStep / 4) g
            return $ TimeStamp $ addUTCTime (fromIntegral $ floor $ v * 60) x
        mutLoc g (Location x y) = do
            dx <- normal genLocStepX (abs genLocStepX / 4) g
            dy <- normal genLocStepY (abs genLocStepY / 4) g
            let x' = genSystemSize + x + round (dx :: Double)
            let y' = genSystemSize + y + round dy
            return $ Location (x' `mod` genSystemSize)
                              (y' `mod` genSystemSize)
        mutTemp g x = do
            dx <- normal 0 genTempStep g
            return $ x + round dx
        mutLine g x = do
            c <- uniform g
            return $ if c < genFailRate then Invalid "th1s1s1nv4l1d" else x
        systemTime   = genStartTime
        balloonLoc   = Location (genSystemSize `div` 2) (genSystemSize `div` 2)
        balloonRange = genBalloonRange
        systemTemp   = 300
        systemObss   = observatoryLocs
        systemSize   = genSystemSize
    in  (Mutators{..}, System{..})

stepSystem :: GenIO -> Mutators -> System -> IO System
stepSystem g Mutators{..} System{..} = do
    newTime <- mutTime g systemTime
    newLoc  <- mutLoc  g balloonLoc
    newTemp <- mutTemp g systemTemp
    return $ System newTime newLoc balloonRange newTemp systemObss systemSize

runSystem :: GenIO -> Mutators -> System -> Producer System IO ()
runSystem g m s = do
    yield s
    s' <- liftIO $ stepSystem g m s
    runSystem g m s'

outputSystem :: GenIO -> Mutators -> Pipe System ByteString IO ()
outputSystem g Mutators{..} = forever $ do
    System{..} <- await
    let systemBounds = Location systemSize systemSize
    let inBounds x = distanceSquared (Just systemBounds) balloonLoc x < (balloonRange * balloonRange)
    let inRange = filter (inBounds . snd) systemObss
    let obsLine = LogLine systemTime balloonLoc systemTemp
    let rawLines = map (Valid . convertMetreKelvinToObservatory . obsLine . fst) inRange
    logLines <- liftIO $ mapM (mutLine g) rawLines
    -- We filter because Data.Csv is fickle when dealing with commas regardless of delimeter
    -- A custom encoder might be faster, but current speed seems more than adequate
    each logLines >-> encodeWith weatherEncodeOptions >-> P.map (BSC.filter (/= '"'))

main :: IO ()
main = do
    settings <- execParser (info parseGenSettings fullDesc)
    g <- createSystemRandom
    let (m, s) = initialise settings
    withFile (genFile settings) WriteMode $ \h ->
         runEffect $ runSystem g m s >-> outputSystem g m >-> P.take (genNumLines settings) >-> P.toHandle h
