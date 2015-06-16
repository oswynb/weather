module Conversion where

import           Types

rawConvertTemp :: TemperatureUOM
               -> TemperatureUOM
               -> Double
               -> Double
rawConvertTemp oldUOM newUOM = fromCelsius newUOM . toCelsius oldUOM
  where
    toCelsius :: TemperatureUOM -> Double -> Double
    toCelsius   Celsius    x = x
    toCelsius   Fahrenheit x = (x - 32) / 1.8
    toCelsius   Kelvin     x = x - 273.15
    fromCelsius Celsius    x = x
    fromCelsius Fahrenheit x = 1.8 * x + 32
    fromCelsius Kelvin     x = x + 273.15

convertTemp :: TemperatureUOM
            -> TemperatureUOM
            -> Temperature
            -> Temperature
convertTemp oldUOM newUOM = round . rawConvertTemp oldUOM newUOM . fromIntegral

rawConvertLen :: LocationUOM
              -> LocationUOM
              -> Double
              -> Double
rawConvertLen oldUOM newUOM = fromMetres newUOM . toMetres oldUOM
  where
    toMetres :: LocationUOM -> Double -> Double
    toMetres   Metre     x = x
    toMetres   Kilometre x = x * 1000
    toMetres   Mile      x = x * 1609.344
    fromMetres Metre     x = x
    fromMetres Kilometre x = x / 1000
    fromMetres Mile      x = x / 1609.344


convertLen :: Integral a
           => LocationUOM
           -> LocationUOM
           -> a
           -> a
convertLen oldUOM newUOM = round . rawConvertLen oldUOM newUOM . fromIntegral

convertLoc :: LocationUOM
           -> LocationUOM
           -> Location
           -> Location
convertLoc oldUOM newUOM (Location x y) =
    let f = convertLen oldUOM newUOM
    in Location (f x) (f y)

convertLine :: LogLine
            -> LocationUOM
            -> TemperatureUOM
            -> LogLine
convertLine (LogLine ts loc temp obs) newLocUOM newTempUOM =
    let oldLocUOM  = observatoryLocationUOM    obs
        oldTempUOM = observatoryTemperatureUOM obs
        newLoc     = convertLoc  oldLocUOM  newLocUOM  loc
        newTemp    = convertTemp oldTempUOM newTempUOM temp
    in LogLine ts newLoc newTemp obs

statsLine :: LogLine
          -> StatsLine
statsLine (LogLine _ (Location x y) temp obs) =
    let oldLocUOM  = observatoryLocationUOM    obs
        oldTempUOM = observatoryTemperatureUOM obs
        f          = rawConvertLen oldLocUOM Metre . fromIntegral
        newLoc     = (f x, f y)
        newTemp    = rawConvertTemp oldTempUOM Kelvin $ fromIntegral temp
    in StatsLine newLoc newTemp obs

convertMetreKelvinToObservatory :: LogLine
                                -> LogLine
convertMetreKelvinToObservatory (LogLine ts loc temp obs) =
    let newLocUOM  = observatoryLocationUOM obs
        newTempUOM = observatoryTemperatureUOM obs
        newLoc     = convertLoc  Metre  newLocUOM  loc
        newTemp    = convertTemp Kelvin newTempUOM temp

    in LogLine ts newLoc newTemp obs
