
normalise :: (Double, Double, Double) -> (Double, Double, Double)
normalise (x,y,z) = (x / norm, y / norm, z / norm) where
    norm :: Double
    norm = sqrt (xˆ2 + yˆ2 + zˆ2)




