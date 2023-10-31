module Week36Exercise2 where

semiRepetitive :: String -> Maybe String
semiRepetitive x =
    let len = length x
        midpoint = len `div` 2
        firstHalf = take midpoint x
        lastHalf = drop (len - midpoint) x
    in
        if firstHalf == lastHalf
            then Just firstHalf
            else Nothing

decomposeSemiRepetitive :: String -> Maybe (String, Maybe Char)
decomposeSemiRepetitive x =
    let len = length x
        midpoint = len `div` 2
        middleChar = x !! midpoint
        firstHalf = take midpoint x
        lastHalf = drop (len - midpoint) x
    in
        if firstHalf == lastHalf
            then
                if len `mod` 2 == 1
                    then Just (firstHalf, Just middleChar)
                    else Just (firstHalf, Nothing)
            else Nothing

createSemiRepetitive :: String -> Maybe Char -> String
createSemiRepetitive x y =
    let
        len = length x
        thiny = x ++ x
        middleChar = case y of
            Just c -> [c]
            Nothing -> ""
    in
        if len `mod` 2 == 1 || even len
            then x ++ middleChar ++ x
            else x ++ x








