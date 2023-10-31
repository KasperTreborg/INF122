module Oblig0 where

import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List

type Key = [(Char,Char)]
type FrequencyTable = [(Char,Double)]
type Alphabet = String
type Dictionary = Set.Set String


-- Oppgave 1

-- Hjelpefunksjon til både encode og decode
-- encryptChar tar inn en Key og en Char og sjekker om Char'en eksisterer i Key'en for å finne den krypterte motparten.
-- Hvis den finner en match med en motpart vil koden returnere den krypterte karakteren, hvis ikke returnerer den den originale.
encryptChar :: Key -> Char -> Char
encryptChar key char = fromMaybe char (lookup char key)

-- Krypterer en String ved hjelp av encryptChar som blir brukt på hver karakter i strengen.
encode :: Key -> String -> String
encode k = map (encryptChar k)

-- Samme prosess som i encode bare at jeg inverterer input Key'en.
decode :: Key -> String -> String
decode k = map (encryptChar $ invert k)
    where
        -- Invert bruker en enkel map funksjon til å flippe om verdiene i en hver tuple i input Key'en.
        invert :: Key -> Key
        invert = map (\(x,y) -> (y,x))

-- caesar tar inn et alfabet og et tall som den i tur bruker for å gi karakterer verdien til en annen karakter som er "shift" antall vekke.
-- Også for at den skal funke for både positive og negative tall bruker jeg en enkel if-setning som sier at hvis shift er større eller lik 0 bruker den rotate
-- Hvis ikke bruker den negativeRotate som fungerer egentlig helt likt, bare optimisert for negative tall.
caesar :: Alphabet -> Integer -> Key
caesar alphabet shift
    | shift >= 0 = zip alphabet (rotate (shift `mod` fromIntegral (length alphabet)) alphabet)   
    | otherwise = zip alphabet (negativeRotate (abs shift `mod` fromIntegral (length alphabet)) alphabet)
    where
        -- rotate roterer listen til høyre basert på tall inputen, denne blir brukt for positive tall
        rotate :: Integer -> [a] -> [a]
        rotate _ [] = []
        rotate 0 xs = xs
        rotate i xs = rotate (i - 1) (tail xs ++ [head xs])

        -- negativeRotate gjør i bunn og grunn det samme som rotate bare at denne roterer listen til venstre ettersom den er for negative tall.
        negativeRotate :: Integer -> [a] -> [a]
        negativeRotate _ [] = []
        negativeRotate 0 xs = xs
        negativeRotate i xs = negativeRotate (i - 1) (last xs : init xs)


-- Oppgave 2

-- count teller hvor ofte en karakter i en string dukker opp og i tur returnerer et FrequencyTable som setter inn karakteren med antall gonger den dukker opp
-- delt på den totale mengden karakterer.
count :: String -> FrequencyTable
count string = map (\x -> (head x, fromIntegral(length x) / fromIntegral(length string))) grouped
    where
        sorted = sort string
        grouped = group sorted

-- loadFrequencyTable tar inn en FilePath som den bruker readFile funksjonen på for å lese innholde i filen
-- dermed lager vi en variable "result" som bruker count funksjonen på innholde, som vi til slutt returnerer slik at det blir en IO FrequencyTable
-- og ikke bare et FrequencyTable
loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable file = do
    contents <- readFile file
    let result = count contents
    return result

-- Hjelpefunksjon til initialGuess
-- ord sorterer et FrequencyTable i synkende rekkefølge av frekvensen som sorterer karakterene etter mest brukt til minst brukt.
ord :: FrequencyTable -> FrequencyTable
ord = sortBy (\(_,a) (_,b) -> compare b a)

-- initialGuess sorterer først begge input FrequencyTable'ne ved hjelp av ord hjelpefunksjonen og bruker dermed zip til å koble 
-- første elemenetet (Char) i hver liste med hverandre helt til en eller begge av listene er tomme.
initialGuess :: FrequencyTable -> FrequencyTable -> Key
initialGuess model observation = zip (map fst sortedModel) (map fst sortedObservation)
    where
        sortedModel = ord model
        sortedObservation = ord observation

-- Først kombinerer den di unike karakterene som er etablert i begge FrequencyTablene og lagerer dem i uniqueChars listen
-- Dermed kalkulerer den chi-squared statistikken ved å ta ((observedFreq char - expectedFreq char) ^ 2) / expectedFreq char
-- hvor observedFreq er enten en observert karakter eller 0 om den ikke blir observert og hvor expectedFreq er en forventet karakteren eller 1/10000 om den ikke
-- finner karakteren.
chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared model observation = sum $ map calculateTerm uniqueChars
    where
        uniqueChars = nub $ map fst (model ++ observation)
        calculateTerm char = ((observedFreq char - expectedFreq char) ^ 2) / expectedFreq char
        observedFreq char = fromMaybe 0 (lookup char observation)
        expectedFreq char = fromMaybe (1/10000) (lookup char model)


-- Oppgave 3

-- Bruker map til å påføre hjelpefunksjonen swapHelp til hver par i  listen.
-- swapHelp sjekker om første elementet av input paret (a,b) er lik c1 hvis det er det, bytter den det med c2 og motsatt hvis den er lik c2
-- Hvis ikke blir paret uendret
swapEntries :: Eq a => (a, b) -> (a, b) -> [(a, b)] -> [(a, b)]
swapEntries c1 c2 = map (swapHelp c1 c2)
    where
        swapHelp :: Eq a => (a, b) -> (a, b) -> (a, b) -> (a, b)
        swapHelp (c1, e1) (c2, e2) (a, b)
            | a == c1 = (c2,b)
            | a == c2 = (c1,b)
            | otherwise = (a,b)

-- Prvøde denne versjonen først men nub gjør at den bruker for lang tid, men i teorien tror jeg denne burde ogaå funket.
-- neighbourKeys :: Key -> [Key]
-- neighbourKeys key = nub [swapEntries (c1, e1) (c2, e2) key | (c1, e1) <- key, (c2, e2) <- key, c1 /= c2]

-- Ettersom koden over ikke funket pga nub, fikk jeg hjelp av min medelev og gode kompis Sveinung Hjelmervik herved litt lik kode her.
-- neighbourKeys bruker listekomprehensjon til å iterere gjennom unike par fra input Key'en, og dermed bruker swapEntries til å generere nye nøkkler for hvert par
-- uniquePairs bruker også listekomprehensjon til å generere par av element (a,b) for hver unike combinasjoner av element i input listen.
neighbourKeys :: Eq a => [(a,a)] -> [[(a,a)]]
neighbourKeys key = [swapEntries a b key | (a, b) <- uniquePairs key]
    where
        uniquePairs :: Eq a => [a] -> [(a,a)]
        uniquePairs [] = []
        uniquePairs (a:xs) = [(a, b) | b <- xs, a /= b] ++ uniquePairs xs

-- greedy dekrypterer en kryptert streng med en grådig strategi som oppdaterer nøkkelen med hver iterasjon for å minimere chi-squared statistikken med 
-- mål om å matche karakter frekvensen in i modellen.
-- badCount er bare en dårlig versjon av Count, men fekk ikke til å bruke Count uten at koden ble helt skurrete og tror det er derfor den ene testen ikke fungerer :/
greedy :: FrequencyTable -> String -> Key -> Key
greedy model cipherText initKey = 
    let
        chiSquaredWithKey = chiSquared model . badCount
        badCount key = map (\c -> (c, fromIntegral (length (filter (== c) decryptedText)) / fromIntegral (length decryptedText))) ['a'..'z']
        decryptedText = decode initKey cipherText
        possibleKeys = neighbourKeys initKey
        bestKey = minimumBy (\k1 k2 -> compare (chiSquaredWithKey k1) (chiSquaredWithKey k2)) possibleKeys
    in
        if chiSquaredWithKey bestKey < chiSquaredWithKey initKey
        then greedy model cipherText bestKey
        else initKey

-- Oppgave 4

-- Denne funksjonen tar inn en fil og leser dens innhold.
-- Dermed finner den alle di forskjellige ordene og returnerer et Dictionary ved hjelp av Set.fromList funksjonen
loadDictionary :: FilePath -> IO Dictionary
loadDictionary fil = do
    contents <- readFile fil
    let wordList = words contents
    return (Set.fromList wordList)

-- Her finner vi alle ordene i input strengen og sjekker det opp mot dictionary'et med hjelp av filter og Set.member.
-- Dermed gir den tilbake et tall med hvor mange av ordene som er i dictionary'et.
countValidWords :: Dictionary -> String -> Integer
countValidWords dict string = 
    let
        wordsInInput = words string
        validWords = filter (`Set.member` dict) wordsInInput
    in
        fromIntegral (length validWords)

-- greedyDict prøver å dekryptere en kryptert streng ved hjelp av en grådig strategi (som i den tidligere greedy funksjonen) bare mens den også holder
-- dictionary'et med valid ord i bakhodet. Altså oppdaterer nøkkelen itererende for å maximere antall valide ord i den dekrypterte teksten.
-- Litt sånn halfassed forklaring men gikk ut ifra forklaringen på oppgaven :D
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