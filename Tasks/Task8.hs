{-|
Module      : Tasks.Task8
Description : map un concat kompozīcijas likuma pārbaude.

Modulis pārbauda funkcionālās programmēšanas likumu:

@
map f . concat = concat . map (map f)
@

Likums nosaka, ka elementu pārveidošana pēc
sarakstu apvienošanas dod tādu pašu rezultātu
kā katra iekšējā saraksta pārveidošana pirms
apvienošanas.

Pārbaudes funkcija salīdzina abas likuma puses
konkrētām ievadēm.

Matemātiskais pamatojums balstās uz strukturālo
indukciju pār galīgu sarakstu sarakstu.
-}

module Tasks.Task8 
    (checkMapConcatLaw
    , runTask8Tests
    ) where

{-|
Pārbauda map un concat kompozīcijas likumu.

Funkcija aprēķina abas likuma puses un
salīdzina iegūtos rezultātus.

'Eq b' nepieciešams, jo rezultātu elementiem
jāatbalsta vienādības salīdzināšana.

Pārveidošanas funkcija var mainīt elementu tipu,
jo ievades tips @a@ un rezultāta tips @b@
nav obligāti vienādi.

Likuma kreisā puse:
@
(map f . concat) lists
@

Vispirms tiek apvienoti iekšējie saraksti,
pēc tam pārveidots katrs elements.

Likuma labā puse:
@
(concat . map (map f)) lists
@

Vispirms tiek pārveidoti katra iekšējā
saraksta elementi, pēc tam saraksti apvienoti.

Sarežģītība:
Abu pušu aprēķināšanai nepieciešamais laiks: O(n),
kur n ir kopējais elementu skaits.
-}
checkMapConcatLaw :: Eq b => (a -> b) -> [[a]] -> Bool

checkMapConcatLaw f lists =

    -- Salīdzinā divas ekvivalentas darbību secības,
    -- lai pārbaudītu likumu konkrētajai ievadei
    (map f . concat) lists ==
    (concat . map (map f)) lists


-- Viena testa rezultāta pārbaude.
-- Sagaidām True, jo map un concat likumam
-- jāizpildās visiem galīgiem sarakstiem.
testMapConcat :: String -> Bool -> IO Bool

testMapConcat testName actual = do
    let expected = True
    let passed = actual == expected

    putStrLn ("Test: " ++ testName)
    putStrLn ("Expected: " ++ show expected)
    putStrLn ("Actual:   " ++ show actual)
    putStrLn ("Result: " ++ if passed then "PASS" else "FAIL")
    putStrLn ""

    return passed

{-|
Piecu robežgadījumu testu demonstrācija.

Pārbaudes ietver dažādus datu tipus,
tukšu iekšējo sarakstu un tukšu ārējo sarakstu.
-}
runTask8Tests :: IO ()
runTask8Tests = do
    putStrLn "Task 8: Edge Case Tests"
    putStrLn ""

    results <- sequence
        [ testMapConcat "Multiply by 2"
            (checkMapConcatLaw (*2) [[1,2],[3,4]])

        , testMapConcat "Empty inner list"
            (checkMapConcatLaw (+10) [[1],[2,3],[]])

        , testMapConcat "String lengths"
            (checkMapConcatLaw length
                [["Hi","Hello"],["Haskell"]])

        , testMapConcat "Boolean negation"
            (checkMapConcatLaw not
                [[True,False],[False]])

        , testMapConcat "Empty outer list"
            (checkMapConcatLaw (+1) ([] :: [[Int]]))
        ]

    let passed = length (filter id results)
    let total = length results

    putStrLn ("Tests passed: " ++ show passed ++ "/" ++ show total)