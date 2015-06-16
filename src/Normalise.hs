{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as BSC
import           Options.Applicative
import           Pipes
import qualified Pipes.ByteString      as P (fromHandle, toHandle)
import           Pipes.Csv
import qualified Pipes.Prelude         as P hiding (fromHandle, toHandle)
import           System.IO

import           Conversion
import           Parsing
import           Types

data NormaliseSettings = NormaliseSettings
  { outputTempUOM :: TemperatureUOM
  , outputLocUOM  :: LocationUOM
  , inputFile     :: FilePath
  , outputFile    :: FilePath
  }

parseNormaliseSettings :: Parser NormaliseSettings
parseNormaliseSettings = NormaliseSettings
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
         <> short 'i'
         <> metavar "INPUT_FILE"
         <> help "File to read data from")
    <*> strOption
        (   long "input-file"
         <> short 'o'
         <> metavar "OUTPUT_FILE"
         <> help "File to write data to")

main :: IO ()
main = do
    NormaliseSettings{..} <- execParser (info parseNormaliseSettings fullDesc)
    withFile inputFile ReadMode $ \h1 ->
        withFile outputFile WriteMode $ \h2 -> runEffect $
            decodeWith weatherDecodeOptions NoHeader (P.fromHandle h1) >->
            filterRight >->
            P.map (\l -> convertLine l outputLocUOM outputTempUOM) >->
            encodeWith weatherEncodeOptions >->
            -- We filter because Data.Csv is fickle when dealing with commas
            -- regardless of delimeter. A custom encoder might be faster,
            -- but current speed seems more than adequate
            P.map (BSC.filter (/= '"')) >->
            P.toHandle h2
  where
    filterRight :: Pipe (Either String LogLine) LogLine IO ()
    filterRight = forever $ do
        x <- await
        case x of
            Left _   -> return ()
            Right x' -> yield x'
