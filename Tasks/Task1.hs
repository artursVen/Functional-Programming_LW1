{-|
Module      : Tasks.Task1
Description : Maksimālā skaitļa meklēšana veselu skaitļu sarakstā

Modulis nodrošina funkciju lielākā skaitļa noteikšanai
veselu skaitļu sarakstā

Algoritms izmanto dilstošu kārtošanu, jo uzdevuma nosacījums
ir noteikt maksimumu ar kārtošanas palīdzību

Funkcija izmanto 'Maybe', lai tukša saraksta gadījumā
rezultātu varētu apstrādāt bez izņēmuma
-}

module Tasks.Task1 
    (findMaximumNumber
    , testFindMaximum
    ) where

import Data.List (sortBy)
import Data.Ord (comparing, Down(..))


{-|
Atrod lielāko veselo skaitli sarakstā.

Funkcija ir pilnīga attiecībā uz galīgiem sarakstiem:
tā atgriež rezultātu arī tad, ja ievades saraksts ir tukšs.

Parametri:
* @numbers@ - veselu skaitļu saraksts, kurā meklē maksimumu.

Atgrieztā vērtība:
* @Just maximumNumber@ - ja sarakstā ir vismaz viens elements.
* @Nothing@ - ja saraksts ir tukšs.

Sarežģītība:
* Laika sarežģītība: \(O(n \log n)\).
-}

findMaximumNumber :: [Int] -> Maybe Int

-- Tukšam sarakstam maksimums nav definēts
-- Nothing ļauj izsaucējam šo gadījumu apstrādāt droši
findMaximumNumber [] = Nothing


findMaximumNumber numbers = 
    case sortBy (comparing Down) numbers of

        -- Dilstoša secība garantē, ka pirmais elements ir maksimālais
        (maximumNumber : _) -> Just maximumNumber

        -- Saglabājam gadījumu apstrādi,
        -- lai funkcija neizraisītu pattern-match kļūdu.
        [] -> Nothing


-- Testēšanas funkcija
testFindMaximum :: String -> [Int] -> Maybe Int -> IO ()
testFindMaximum testName input expected = do
    let actual = findMaximumNumber input

    putStrLn ("Test: " ++ testName)
    putStrLn ("Input: " ++ show input)
    putStrLn ("Expected: " ++ show expected)
    putStrLn ("Actual: " ++ show actual)

    if actual == expected
        then putStrLn "Result: PASS"
        else putStrLn "Result: FAIL"

    putStrLn ""