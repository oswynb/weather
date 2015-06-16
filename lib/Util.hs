module Util where

import           Types

rawDistance :: (Double, Double) -> (Double, Double) -> Double
rawDistance (x1, y1) (x2, y2) =
    let dx = abs (x1 - x2)
        dy = abs (y1 - y2)
    in sqrt (dx*dx + dy*dy)

-- Taking an option world size parameter calculate the distance between two points
-- If no world size parameter is given, assume world is flat
distanceSquared :: Maybe Location -> Location -> Location -> Double
distanceSquared Nothing (Location x1 y1) (Location x2 y2) =
    let dx = abs (x1 - x2)
        dy = abs (y1 - y2)
    in fromIntegral $ dx * dx + dy * dy
distanceSquared (Just (Location bx by)) (Location x1 y1) (Location x2 y2) =
    let dx = abs (x1 - x2)
        dy = abs (y1 - y2)
        dx' = min dx (bx - dx)
        dy' = min dy (by - dy)
    in fromIntegral $ dx' * dx' + dy' * dy'

distance :: Maybe Location -> Location -> Location -> Double
distance b l1 l2 = sqrt $ distanceSquared b l1 l2
