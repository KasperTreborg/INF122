module Week40Exercise0 where

data SemiRepetitive = SR String (Maybe Char)

semiRepetitive :: String -> Maybe SemiRepetitive
semiRepetitive str =
    let len = length str
        midpoint = len `div` 2
        middleChar = str !! midpoint
        firstHalf = take midpoint str
        lastHalf = drop (len - midpoint) str
    in
        if firstHalf == lastHalf
        then
            if len `mod` 2 == 1
                then Just $ SR firstHalf (Just middleChar)
                else Just $ SR firstHalf Nothing
        else Nothing

toString :: SemiRepetitive -> String
toString (SR str c) = 
    let
        middleChar = case c of
            Just c -> [c]
            Nothing -> ""
    in
        str ++ middleChar ++ str