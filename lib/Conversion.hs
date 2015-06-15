module Conversion where

import           Types

convertTemp :: TemperatureUOM
            -> TemperatureUOM
            -> Temperature
            -> Temperature
convertTemp oldUOM newUOM = round . fromCelsius newUOM . toCelsius oldUOM . fromIntegral
  where
    toCelsius :: TemperatureUOM -> Double -> Double
    toCelsius   Celsius    x = x
    toCelsius   Fahrenheit x = (x - 32) / 1.8
    toCelsius   Kelvin     x = x - 273.15
    fromCelsius Celsius    x = x
    fromCelsius Fahrenheit x = 1.8 * x + 32
    fromCelsius Kelvin     x = x + 273.15

convertLen :: Integral a
           => LocationUOM
           -> LocationUOM
           -> a
           -> a
convertLen oldUOM newUOM = round . fromMetres newUOM . toMetres oldUOM . fromIntegral
  where
    toMetres :: LocationUOM -> Double -> Double
    toMetres   Metre     x = x
    toMetres   Kilometre x = x * 1000
    toMetres   Mile      x = x * 1609.344
    fromMetres Metre     x = x
    fromMetres Kilometre x = x / 1000
    fromMetres Mile      x = x / 1609.344

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

convertMetreKelvinToObservatory :: LogLine
                                -> LogLine
convertMetreKelvinToObservatory (LogLine ts loc temp obs) =
    let newLocUOM  = observatoryLocationUOM obs
        newTempUOM = observatoryTemperatureUOM obs
        newLoc     = convertLoc  Metre  newLocUOM  loc
        newTemp    = convertTemp Kelvin newTempUOM temp

    in LogLine ts newLoc newTemp obs
