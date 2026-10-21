{-|
Module      : Tasks.Task2
Description : Teksta virknes simbolu secības apgriešana

Modulis nodrošina tīru funkciju teksta virknes simbolu 
secības apgriešanai.

Tiek izmantota Haskell funkcija 'reverse', lai izvairītos 
no atkārtotas algoritma implementēšanas.

Funkcija neapstrādā lietotāja ievadi un neveic izvadi.
-}

module Tasks.Task2 
    (reverseInput
    , runTask2Tests
    ) where

{-|
Apgriež teksta virknes simbolu secību.

Funkcija saglabā visus ievades simbolus, tai skaitā
atstarpes, ciparus un pieturzīmes, mainot tikai to secību.

Tukša virkne ir derīga ievade, rezultāts ir tukša virkne.

Parametri:
* @input@ - sākotnējā teksta virkne.

Atgrieztā vērtība:
* Teksta virkne ar apgrieztu simbolu secību.

Sarežģītība:

Laika sarežģītība: O(n).
-}

reverseInput :: String -> String
reverseInput input = reverse input


-- Izpilda viena testa sagaidāmo un faktisko rezultātu.
testReverseInput :: String -> String -> String -> IO Bool
testReverseInput testName input expected = do

    let actual = reverseInput input
    let passed = actual == expected

    putStrLn ("Test: " ++ testName)
    putStrLn ("Input:    " ++ show input)
    putStrLn ("Expected: " ++ show expected)
    putStrLn ("Actual:   " ++ show actual)

    if passed
        then putStrLn "Result: PASS"
        else putStrLn "Result: FAIL"

    putStrLn ""

    return passed

-- Piecu robežgadījumu testu demonstrācija
runTask2Tests :: IO ()
runTask2Tests = do

    putStrLn ""
    putStrLn "Task 2: Edge Case Tests"
    putStrLn ""

    results <- sequence
        [ 
        testReverseInput "Normal word"
        "Haskell" "lleksaH"

        , testReverseInput "Empty string"
            "" ""

        , testReverseInput "Single character"
            "A" "A"

        , testReverseInput "Palindrome"
            "level" "level"

        , testReverseInput "Spaces between words"
            "Hello World" "dlroW olleH"
        ]

    let passed = length (filter id results)
    let total = length results

    putStrLn ("Tests passed: " ++ show passed ++ "/" ++ show total)
