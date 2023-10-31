module Oppgave1 where

import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List

type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set.Set String

-- Oppgave 1

encryptChar :: Key -> Char -> Char
encryptChar key char = fromMaybe char (lookup char key)

encode :: Key -> String -> String
encode k = map (encryptChar k)

decode :: Key -> String -> String
decode k = map (encryptChar $ invert k)
    where
        invert :: Key -> Key
        invert = map (\(x,y) -> (y,x))

caesar :: Alphabet -> Integer -> Key
caesar alph i = zip alph (rotate i alph)
    where
        rotate :: Integer -> [a] -> [a]
        rotate _ [] = []
        rotate 0 xs = xs
        rotate i xs = rotate (i - 1) (tail xs ++ [head xs])


-- Oppgave 2

count :: String -> FrequencyTable
count string = map (\x -> (head x, fromIntegral(length x) / fromIntegral(length string))) grouped
    where
        sorted = sort string
        grouped = group sorted

loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable file = do
    contents <- readFile file
    let result = count contents
    return result

ord :: FrequencyTable -> FrequencyTable
ord = sortBy (\(_,a) (_,b) -> compare b a)

initialGuess :: FrequencyTable -> FrequencyTable -> Key
initialGuess model observation = zip (map fst sortedModel) (map fst sortedObservation)
    where
        sortedModel = ord model
        sortedObservation = ord observation

chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared model observation = sum $ map calculateTerm uniqueChars
    where
        uniqueChars = nub $ map fst (model ++ observation)
        calculateTerm char = (observedFreq char - expectedFreq char) ^ 2
        observedFreq char = fromMaybe 0 (lookup char observation)
        expectedFreq char = fromMaybe (1/10000) (lookup char model)


-- Oppgave 3

switchy :: Eq a => (a, a) -> (a, a) -> (a, a) -> (a, a)
switchy (c1, e1) (c2, e2) (a, b)
    | a == c1 = (c2,b)
    | a == c2 = (c1,b)
    | otherwise = (a,b)

swapEntries :: Eq a => (a, a) -> (a, a) -> [(a, a)] -> [(a, a)]
swapEntries c1 c2 = map (switchy c1 c2)

neighbourKeys :: Key -> [Key]
neighbourKeys key = nub [swapEntries (c1, e1) (c2, e2) key | (c1, e1) <- key, (c2, e2) <- key, c1 /= c2]

greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey = 
    let
        chiSquaredWithKey = chiSquared model . frequencyTable
        frequencyTable key = map (\c -> (c, fromIntegral (length (filter (== c) decryptedText)) / fromIntegral (length decryptedText))) ['a'..'z']
        decryptedText = decode initKey cipherText
        possibleKeys = neighbourKeys initKey
        bestKey = minimumBy (\k1 k2 -> compare (chiSquaredWithKey k1) (chiSquaredWithKey k2)) possibleKeys
    in
        if chiSquaredWithKey bestKey < chiSquaredWithKey initKey
        then greedy model cipherText bestKey
        else initKey


-- Oppgave 4

loadDictionary :: FilePath -> IO Dictionary
loadDictionary fil = do
    fileContent <- readFile fil
    let wordList = words fileContent
    return (Set.fromList wordList)

countValidWords :: Dictionary -> String -> Integer
countValidWords dict string = 
    let
        wordsInInput = words string
        validWords = filter (`Set.member` dict) wordsInInput
    in
        fromIntegral (length validWords)

greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dict cipherText initKey = 
    let
        validWordsWithKey key = countValidWords dict (decode key cipherText)
        possibleKeys = neighbourKeys initKey
        bestKey = maximumBy (compareKeys validWordsWithKey) possibleKeys
        compareKeys f k1 k2 = compare (f k1) (f k2)
    in
        if validWordsWithKey bestKey > validWordsWithKey initKey
        then greedyDict dict cipherText bestKey
        else initKey