

f :: Integer -> Integer
f n = (n * (n + 1)) `div` 2

triangleNumber :: Integer -> Integer
triangleNumber n = sum [1..n]

palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ reverse xs

f1Week36 :: String -> Char -> Bool
f1Week36 s c = not $ elem c s

f2Week36 :: [Integer] -> [t] -> [(Integer, t)]
f2Week36 tall t =
    let
        yeo = reverse tall
    in
        zip yeo t

semiRepetitive :: String -> Maybe String
semiRepetitive s =
    let
        firstHalf = take (length s `div` 2) s
        lastHalf = drop (length s `div` 2) s
    in
        if firstHalf == lastHalf
            then Just firstHalf
            else Nothing

decomposeSemiRepetitive :: String -> Maybe (String, Maybe Char)
decomposeSemiRepetitive s =
    let
        len = length s
        mid = len `div` 2
        firstHalf = take mid s
        lastHalf = drop (len - mid) s
        middleChar = if odd len then Just (s !! mid) else Nothing
    in
        if firstHalf == lastHalf
            then Just (firstHalf, middleChar)
            else Nothing

createSemiRepetitive :: String -> Maybe Char -> String
createSemiRepetitive s c = 
    let
        mid = case c of
            Just c -> [c]
            Nothing -> ""
    in
        s ++ mid ++ s

information :: [String] -> [String] -> [Integer] -> [String]
information navn studie aar = undefined