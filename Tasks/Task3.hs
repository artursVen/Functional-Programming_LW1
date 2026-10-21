{-|
Module      : Tasks.Task3
Description : Teksta vārdu pirmo burta pārveidošana par lielo burtu.

Modulis nodrošina tīras funkcijas viena vārda un
visa teikuma apstrādei.

Teikuma apstrāde ir sadalīta divās funkcijās, lai viena vārda 
pārveidošanas loģiku varētu izmantot un pārbaudīt neatkarīgi.

Tiek izmantotas Haskell funkcijas 'words', 'map' un 'unwords', 
lai izvairītos no atkārtotas algoritma implementēšanas.
-}

module Tasks.Task3
    ( capitalizeInputWord
    , capitalizeInputSentence
    , runTask3Tests
    ) where

import Data.Char (toUpper)

{-|
Pārveido vārda pirmo simbolu par lielo burtu.

Pārējie simboli tiek saglabāti bez izmaiņām.
Funkcija apstrādā arī tukšu virkni, tādēļ nav nepieciešama 
atsevišķa kļūdu apstrāde.

Parametri:
* @input@ - apstrādājamais vārds.

Atgrieztā vērtība:
* Virkne ar pārveidotu pirmo simbolu.
* Tukšas ievades gadījumā tukša virkne.

Sarežģītība:
Pirmā elementa apstrāde aizņem O(1) laiku.
-}

capitalizeInputWord :: String -> String

-- Drošības pārbaude.
-- Funkcija ir definēta arī ievadei, kurā nav neviena simbola.
capitalizeInputWord [] = []
capitalizeInputWord (firstChar : rest) = toUpper firstChar : rest

{-|
Pārveido katra teikuma vārda pirmo simbolu par lielo burtu.

Funkcija izmanto viena vārda apstrādes funkcijas loģiku.

'words' sadala ievadi pēc atstarpju simboliem,
'unwords' savieno rezultātu ar vienu atstarpi.

Sākotnējais atstarpju formatējums netiek saglabāts.

Parametri:
* @sentence@ - apstrādājamais teikums.

Atgrieztā vērtība:
* Teikums ar pārveidotu katra vārda pirmo simbolu.
* Tukšai ievadei vai tikai atstarpēm - tukša virkne.

Sarežģītība
* Laika sarežģītība: O(n).
* Papildu atmiņas sarežģītība: O(n).
kur n ir ievades virknes simbolu skaits.
-}
capitalizeInputSentence :: String -> String

capitalizeInputSentence sentence = 
    unwords (map capitalizeInputWord (words sentence))


-- Izpilda viena testa sagaidāmo un faktisko rezultātu.
testCapitalizeInputSentence :: String -> String -> String -> IO Bool
testCapitalizeInputSentence testName input expected = do
    let actual = capitalizeInputSentence input
    let passed = actual == expected

    putStrLn ("Test: " ++ testName)
    putStrLn ("Input:    " ++ show input)
    putStrLn ("Expected: " ++ show expected)
    putStrLn ("Actual:   " ++ show actual)
    putStrLn ("Result: " ++ if passed then "PASS" else "FAIL")
    putStrLn ""

    return passed

-- Piecu robežgadījumu testu demonstrācija
runTask3Tests :: IO ()
runTask3Tests = do
    putStrLn "--- Task 3: Edge Case Tests ---"
    putStrLn ""

    results <- sequence
        [ testCapitalizeInputSentence "Empty input"
            "" ""

        , testCapitalizeInputSentence "Single character"
            "a" "A"

        , testCapitalizeInputSentence "Multiple spaces"
            "hello   world" "Hello World"

        , testCapitalizeInputSentence "Mixed case"
            "hELLO wORLD" "HELLO WORLD"

        , testCapitalizeInputSentence "Numbers at word start"
            "123abc test" "123abc Test"
        ]

    let passed = length (filter id results)
    let total = length results

    putStrLn ("Tests passed: " ++ show passed ++ "/" ++ show total)