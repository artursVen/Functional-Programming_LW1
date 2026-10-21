{-|
Module      : Tasks.Task6
Description : Visu saraksta variantu ģenerēšana izņemot vienu elementu.

Modulis nodrošina polimorfu funkciju, kas ģenerē
visus saraksta variantus, izņemot ārā vienu elementu.

Tiek izmantota rekursija, jo tā ļauj sadalīt uzdevumu divās daļās: 
pirmā elementa izņemšana un atlikušo elementu apstrāde.

Atkārtotas vērtības netiek filtrētas.
-}

module Tasks.Task6 
    (gaps
    , runTask6Tests
    ) where

{-|
Ģenerē visus variantus, kuros no saraksta
izņemts viens elements.

Katrs rezultāta saraksts saglabā sākotnējo
elementu savstarpējo secību.

Tukšam sarakstam rezultāts ir tukšs saraksts.

Parametri:
* Ievades saraksts ar elementiem, kuru tips ir @a@.

Atgrieztā vērtība:
* Saraksts ar visiem viena elementa izņemšanas variantiem.

Sarežģītība:
Laika sarežģītība: O(n^2),
kur n ir sākotnējā saraksta elementu skaits.
-}
gaps :: [a] -> [[a]]

-- Bāzes gadījums. Ja saraksts ir tukšs tad nav arī rezultāta
gaps [] = []

-- Rekursīvais gadījums
-- (x : xs) sadala sarakstu. [1, 2, 3] -> x = 1; xs = [2, 3]
-- gaps xs sadala sarakstu rekursīvi [2, 3] -> [[3], [2]]
-- map (x :) pievieno atpakaļ katram 1: [[3], [2]] -> [[1, 3], [1, 2]]
gaps (x : xs) = xs : map (x :) (gaps xs)



-- Izpilda viena testa sagaidāmo un faktisko rezultātu.
testGaps :: (Eq a, Show a)
         => String -> [a] -> [[a]] -> IO Bool

testGaps testName input expected = do
    let actual = gaps input
    let passed = actual == expected

    putStrLn ("Test: " ++ testName)
    putStrLn ("Input:    " ++ show input)
    putStrLn ("Expected: " ++ show expected)
    putStrLn ("Actual:   " ++ show actual)
    putStrLn ("Result: " ++ if passed then "PASS" else "FAIL")
    putStrLn ""

    return passed


-- Piecu robežgadījumu testu demonstrācija
runTask6Tests :: IO ()
runTask6Tests = do
    putStrLn "Task 6: Edge Case Tests"
    putStrLn ""

    results <- sequence
        [ testGaps "Empty list"
            ([] :: [Int]) []

        , testGaps "Single element"
            [7] [[]]

        , testGaps "Two elements"
            [1,2] [[2],[1]]

        , testGaps "Repeated values"
            [1,1,2] [[1,2],[1,2],[1,1]]

        , testGaps "String elements"
            ["Haskell","Scala","Python"]
            [ ["Scala","Python"]
            , ["Haskell","Python"]
            , ["Haskell","Scala"]
            ]
        ]

    let passed = length (filter id results)
    let total = length results

    putStrLn ("Tests passed: " ++ show passed ++ "/" ++ show total)