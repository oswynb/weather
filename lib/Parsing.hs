{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parsing where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Csv
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Format
import qualified Data.Vector           as V
import           System.Locale

import           Types

-- TimeStamp

instance FromField TimeStamp where
    parseField s = do
        timeStr <- BS.unpack <$> parseField s
        case parseTime defaultTimeLocale tsFormat timeStr of
            Nothing -> mzero
            Just ts -> pure $ TimeStamp ts

instance ToField TimeStamp where
    toField (TimeStamp x) = BS.pack $ formatTime defaultTimeLocale tsFormat x

instance Read TimeStamp where
    readsPrec _ s = maybeToList $ (,"") <$> TimeStamp <$> parse s
      where
        parse = parseTime defaultTimeLocale tsFormat

-- Location

instance FromField Location where
    parseField s =
        case BS.split ',' s of
            [x, y] -> do
                x' <- parseField x
                y' <- parseField y
                if x' >= 0 && y' >= 0 then
                    return $ Location x' y'
                else
                    mzero
            _      -> mzero

-- Observatory

instance FromField Observatory where
    parseField s = pure $ Observatory s

instance ToField Observatory where
    toField (Observatory x) = x

-- Log lines

instance FromRecord LogLine where
    parseRecord v
        | V.length v == 4 =
            LogLine <$> v .! 0
                    <*> v .! 1
                    <*> v .! 2
                    <*> v .! 3
        | otherwise = mzero

instance ToRecord LogLine where
    toRecord (LogLine ts (Location x y) temp obs) = toRecord (ts, toField x <> "," <> toField y, temp, obs)

weatherEncodeOptions :: EncodeOptions
weatherEncodeOptions = defaultEncodeOptions{encDelimiter = fromIntegral (ord '|')}

weatherDecodeOptions :: DecodeOptions
weatherDecodeOptions = defaultDecodeOptions{decDelimiter = fromIntegral (ord '|')}
