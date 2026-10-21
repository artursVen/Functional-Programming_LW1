{-|
Module      : Tasks.Task7
Description : Binārā meklēšanas koka (BST) pārbaude un jaunu
vērtību ievietošana.

Modulis nodrošina binārā koka datu tipu.

BST pārbaude izmanto robežvērtības, jo tikai tiešo
bērnu salīdzināšana nevar atklāt pārkāpumus
dziļākos apakškokos.

Vienādas vērtības tiek atļautas kreisajā apakškokā.
Labajā apakškokā drīkst atrasties tikai lielākas vērtības.

Modulis ietver arī apzināti kļūdainu vērtību ievietošanas
funkciju, lai demonstrētu BST īpašību pārkāpumu.
-}

module Tasks.Task7
    ( Tree(..)
    , isSearchTree
    , insertTree
    , insertTreeWrong
    , insertPreservesSearchTree
    , insertWrongPreservesSearchTree
    , validTree
    , invalidTree
    , runTask7Tests
    ) where

{-|
Bināra koka datu tips.

Katrs mezgls satur vērtību un divus apakškokus.
'Leaf' apzīmē tukšu koku.

Tips ir polimorfs, to var izmantot ar dažādiem datu tipiem.
-}
data Tree a = Branch a (Tree a) (Tree a) | Leaf
    deriving (Show)


{-|
Pārbaude koka atbilstībai BST īpašībām.

Pārbaude izmanto robežvērtības, lai ņemtu vērā
visu mezgla priekšteču noteiktos ierobežojumus.

Vienādas vērtības ir atļautas tikai kreisajā apakškokā.
-}
isSearchTree :: Ord a => Tree a -> Bool
isSearchTree tree = checkTree Nothing Nothing tree

-- Rekursīvi pārbauda mezglu pieļaujamās robežas.
checkTree :: Ord a => Maybe a -> Maybe a -> Tree a -> Bool

-- Tukšs apakškoks nevar pārkāpt BST īpašību.
checkTree _ _ Leaf = True

checkTree lower upper (Branch x left right) = 
        withinLowerBound &&
        withinUpperBound &&
        checkTree lower (Just x) left &&
        checkTree (Just x) upper right
    where

        -- Apakšējā robeža ir stingra, jo labajā
        -- apakškokā dublikāti nav atļauti.
        withinLowerBound = 
            case lower of
                Nothing -> True
                Just low -> x > low

        -- Augšējā robeža pieļauj vienādību, jo
        -- dublikāti tiek ievietoti kreisajā apakškokā.
        withinUpperBound = 
            case upper of
                Nothing -> True
                Just high -> x <= high


{-|
Ievieto vērtību binārā meklēšanas kokā.

Mazākas vai vienādas vērtības tiek ievietotas
kreisajā apakškokā, bet lielākas - labajā.

Funkcija saglabā BST īpašību, ja sākotnējais
koks ir derīgs BST.
-}
insertTree :: Ord a => a -> Tree a -> Tree a

-- Ievietojam jaunu zaru kas satur ievietoto vērtību
insertTree e Leaf = Branch e Leaf Leaf

insertTree e (Branch x left right)

    -- Mazākas vai vienādas vērtības tiek ievietotas kreisajā apakškokā
    | e <= x = 
        Branch x (insertTree e left) right

    -- Lielākas vērtības tiek ievietotas labajā apakškokā
    | e > x = 
        Branch x left (insertTree e right)


{-|
Pārbauda ievietošanas īpašību.

Ja sākotnējais koks ir derīgs BST, tad arī
pēc ievietošanas rezultātam jābūt derīgam BST.

Nederīgam sākotnējam kokam  priekšnosacījums nav izpildīts, 
tāpēc pārbaude atgriež True.
-}
insertPreservesSearchTree :: Ord a => a -> Tree a -> Bool
insertPreservesSearchTree value tree = 
    not (isSearchTree tree) ||
    isSearchTree (insertTree value tree)


{-|
Apzināti kļūdaina ievietošanas funkcija.

Lielākas vērtības tiek ievietotas kreisajā,
nevis labajā apakškokā.

Šī kļūda ir saglabāta demonstrācijas nolūkos,
kā BST īpašību pārbaude var atklāt nepareizu algoritma uzvedību.
-}
insertTreeWrong :: Ord a => a -> Tree a -> Tree a

insertTreeWrong e Leaf = Branch e Leaf Leaf

insertTreeWrong e (Branch x left right)
    | e <= x = 
        Branch x (insertTreeWrong e left) right

    -- Apzināta kļūda:
    -- Lielāka vērtība tiek nepareizi ievietota kreisajā apakškokā.
    | e > x = 
        Branch x (insertTreeWrong e left) right


{-|
Pārbauda kļūdainās ievietošanas funkcijas spēju saglabāt BST īpašību.

Funkcija paredzēta demonstrācijai:
konkrētām ievadēm rezultāts var būt False.
-}
insertWrongPreservesSearchTree :: Ord a => a -> Tree a -> Bool
insertWrongPreservesSearchTree value tree = 
    not (isSearchTree tree) || 
    isSearchTree (insertTreeWrong value tree)

--- TESTA KOKI ---
-- Derīga binārā meklēšanas koka piemērs
validTree :: Tree Int
validTree = 
    Branch 8
        (Branch 3
            (Branch 1 Leaf Leaf)
            (Branch 6 Leaf Leaf))
        (Branch 10
            Leaf
            (Branch 14 Leaf Leaf))

-- Nederīga binārā meklēšanas koka piemērs
invalidTree :: Tree Int
invalidTree = 
    Branch 10
        (Branch 5
            Leaf
            (Branch 12 Leaf Leaf))
        (Branch 15 Leaf Leaf)


-- Viena testa koka faktiskā rezultāta pārbaude.
testBST :: String -> Bool -> Bool -> IO Bool
testBST testName actual expected = do
    let passed = actual == expected

    putStrLn ("Test: " ++ testName)
    putStrLn ("Expected: " ++ show expected)
    putStrLn ("Actual:   " ++ show actual)
    putStrLn ("Result: " ++ if passed then "PASS" else "FAIL")
    putStrLn ""

    return passed


{-|
Piecu robežgadījumu testu demonstrācija

Testi pārbauda tukšu koku, vienu mezglu, dublikātu izvietojumu un 
ievietošanas īpašību.
-}
runTask7Tests :: IO ()
runTask7Tests = do
    putStrLn "Task 7: Edge Case Tests"
    putStrLn ""

    results <- sequence
        [ testBST "Empty tree"
            (isSearchTree (Leaf :: Tree Int))
            True

        , testBST "Single node"
            (isSearchTree (Branch 5 Leaf Leaf))
            True

        , testBST "Duplicate on left"
            (isSearchTree
                (Branch 5 (Branch 5 Leaf Leaf) Leaf))
            True

        , testBST "Duplicate on right"
            (isSearchTree
                (Branch 5 Leaf (Branch 5 Leaf Leaf)))
            False

        , testBST "Insertion preserves BST"
            (insertPreservesSearchTree 7 validTree)
            True
        ]

    let passed = length (filter id results)
    let total = length results

    putStrLn ("Tests passed: " ++ show passed ++ "/" ++ show total)