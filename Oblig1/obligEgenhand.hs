import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List

type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set.Set String

enchar :: Key -> Char -> Char
enchar k c = fromMaybe c (lookup c k)

encode :: Key -> String -> String
encode key = map $ enchar key

decode :: Key -> String -> String
decode key = map $ enchar $ inverse key
    where
        inverse :: Key -> Key
        inverse [(x,y)] = [(y,x)]

caesar :: Alphabet -> Integer -> Key
caesar alph value = zip alph (rotate (value `mod` fromIntegral (length alph)) alph)
    where
        rotate :: Integer -> [a] -> [a]
        rotate _ [] = []
        rotate 0 xs = xs
        rotate i xs = rotate (i - 1) (tail xs ++ [head xs])