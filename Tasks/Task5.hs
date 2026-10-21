{-|
Module      : Tasks.Task5
Description : Saraksta elementa iegūšana pēc indeksa.

Modulis realizē lietotāja definētu saraksta
indeksēšanas operatoru '(!!)'.

Standartbibliotēkas operators tiek paslēpts, operatora nosaukumu
varētu izmantot uzdevuma realizācijai.

Izveidota droša indeksēšanas funkcija 'safeIndex', kas nederīga indeksa
gadījumā atgriež 'Nothing'.
-}

module Tasks.Task5 (
    (!!)
    , safeIndex
    , runTask5Tests
    ) where

import Prelude hiding ((!!))
import Data.Text.Array (run)


{-|
Atgriež saraksta elementu pēc norādītā indeksa.

Indeksēšana sākas no nulles.

Operators izmanto rekursiju, jo saraksts ir saistīta struktūra 
un piekļuve elementam notiek pārskatot iepriekšējos elementus.

Nederīga indeksa gadījumā tiek izraisīts izņēmums.

Sarežģītība:
Laika sarežģītība: O(i), kur i ir indekss.
-}
(!!) :: [a] -> Int -> a

-- Negatīvu indeksu pārbaudām pirms saraksta  apstrādes, 
-- lai kļūdas iemesls būtu viennozīmīgs arī tukšam sarakstam.
xs !! i
    | i < 0 = error "Index cannot be negative"

-- Tukšs saraksts nozīmē, ka pieprasītais elements nav sasniedzams.
[] !! _ = error "Index out of bounds"

-- Kad indekss sasniedz nulli, ir atrasts meklējamais elements.
(x : _) !! 0 = x

-- Samazinām indeksu, pārejot uz saraksta asti,
-- jo katrā rekursijas solī izlaižam vienu elementu.
(_ : xs) !! i = xs !! (i - 1)



-- Droši iegūst saraksta elementu pēc indeksa.
-- Rezultāta tips 'Maybe' ļauj izsaucējam apstrādāt neveiksmīgu indeksēšanu.
safeIndex :: [a] -> Int -> Maybe a

-- Negatīvs indekss nevar būt derīga elementa pozīcija sarakstā.
safeIndex _ i
    | i < 0 = Nothing

safeIndex [] _ = Nothing
safeIndex (x : _) 0 = Just x
safeIndex (_ : xs) i = safeIndex xs (i - 1)



-- Izpilda viena testa  rezultātu.
testSafeIndex :: (Eq a, Show a)
              => String -> [a] -> Int -> Maybe a -> IO Bool

testSafeIndex testName input index expected = do
    let actual = safeIndex input index
    let passed = actual == expected

    putStrLn ("Test: " ++ testName)
    putStrLn ("List:     " ++ show input)
    putStrLn ("Index:    " ++ show index)
    putStrLn ("Expected: " ++ show expected)
    putStrLn ("Actual:   " ++ show actual)
    putStrLn ("Result: " ++ if passed then "PASS" else "FAIL")
    putStrLn ""

    return passed


-- Piecu robežgadījumu testu demonstrācija
runTask5Tests :: IO ()
runTask5Tests = do
    putStrLn "Task 5: Edge Case Tests"
    putStrLn ""

    results <- sequence
        [ testSafeIndex "First element"
            [10,20,30] 0 (Just 10)

        , testSafeIndex "Last element"
            [10,20,30] 2 (Just 30)

        , testSafeIndex "Empty list"
            ([] :: [Int]) 0 Nothing

        , testSafeIndex "Negative index"
            [10,20,30] (-1) Nothing

        , testSafeIndex "Index out of bounds"
            [10,20,30] 3 Nothing
        ]

    let passed = length (filter id results)
    let total = length results

    putStrLn ("Tests passed: " ++ show passed ++ "/" ++ show total)