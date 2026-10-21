{-|
Module      : Tasks.Task4
Description : Saraksta pirmspēdējā elementa atrašana.

Modulis nodrošina polimorfu funkciju saraksta
pirmspēdējā elementa atrašanai.

Tiek izmantota rekursija un šablonu salīdzināšana, 
lai izvairītos no saraksta garuma aprēķināšanas.

Rezultāta tips 'Maybe' ļauj apstrādāt sarakstus, 
kuros ir mazāk nekā divi elementi, neizraisot izpildes laika izņēmumu.
-}

module Tasks.Task4 
    (secondLastElement
    , runTask4Tests
    ) where

{-|
Atgriež saraksta pirmspēdējo elementu.

Funkcija ir polimorfa, tādēļ to var izmantot
ar jebkura tipa elementu sarakstu.

Funkcija izmanto 'Maybe', 
jo tukšam sarakstam un viena elementa sarakstam 
pirmspēdējais elements nav definēts.

Parametri:
* Ievades saraksts ar elementiem, kuru tips ir @a@.

Atgrieztā vērtība:
* @Just x@ - ja sarakstā ir vismaz divi elementi.
* @Nothing@ - ja sarakstā ir mazāk nekā divi elementi.

Sarežģītība:
* Laika sarežģītība: O(n).
-}
secondLastElement :: [a] -> Maybe a

-- Mazāk par divu elementu sarakstu apstrāde
secondLastElement [] = Nothing
secondLastElement [_] = Nothing

-- Divu elementu sarakstu apstrāde
secondLastElement [x, _] = Just x

-- Rekursija. Ignorējam pirmo elementu un turpinam ar atlikušo sarakstu
secondLastElement (_ : xs) = secondLastElement xs

-- Izpilda viena testa sagaidāmo un faktisko rezultātu.
testSecondLast :: (Eq a, Show a)
               => String -> [a] -> Maybe a -> IO Bool

testSecondLast testName input expected = do
    let actual = secondLastElement input
    let passed = actual == expected

    putStrLn ("Test: " ++ testName)
    putStrLn ("Input:    " ++ show input)
    putStrLn ("Expected: " ++ show expected)
    putStrLn ("Actual:   " ++ show actual)
    putStrLn ("Result: " ++ if passed then "PASS" else "FAIL")
    putStrLn ""

    return passed

-- Piecu robežgadījumu testu demonstrācija
runTask4Tests :: IO ()
runTask4Tests = do
    putStrLn "Task 4: Edge Case Tests"
    putStrLn ""

    results <- sequence
        [ testSecondLast "Empty list"
            ([] :: [Int]) Nothing

        , testSecondLast "Single element"
            [10] Nothing

        , testSecondLast "Two elements"
            [10,20] (Just 10)

        , testSecondLast "Repeated values"
            [5,8,8,8] (Just 8)

        , testSecondLast "String elements"
            ["Haskell","Scala","Python"] (Just "Scala")
        ]

    let passed = length (filter id results)
    let total = length results

    putStrLn ("Tests passed: " ++ show passed ++ "/" ++ show total)
