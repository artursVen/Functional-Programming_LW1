import Prelude hiding ((!!))
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Char(toUpper)
import System.Win32 (xBUTTON1)
import GHC.Base (IO(IO))
import Text.Read (readMaybe)
import Distribution.Compat.CharParsing (lower)
import GHC.Exts.Heap (GenClosure(value))

import Tasks.Task1 (findMaximumNumber, testFindMaximum)
import Tasks.Task2 (reverseInput, runTask2Tests)
import Tasks.Task3 (capitalizeInputWord, capitalizeInputSentence, runTask3Tests)
import Tasks.Task4 (secondLastElement, runTask4Tests)
import Tasks.Task5 ((!!), runTask5Tests)
import Tasks.Task6 (gaps, runTask6Tests)
import Tasks.Task7
    ( Tree(..)
    , isSearchTree
    , insertTree
    , insertTreeWrong
    , insertPreservesSearchTree
    , insertWrongPreservesSearchTree
    , validTree
    , invalidTree
    , runTask7Tests
    )
import Tasks.Task8 (checkMapConcatLaw, runTask8Tests)


task1 :: IO ()
task1 = do

    putStrLn "--- Task 1: Find Maximum ---"
    putStrLn ""

    testFindMaximum "Normal list"
        [5,10,11,15,3,8] (Just 15)

    testFindMaximum "Empty list"
        [] Nothing

    testFindMaximum "Single element"
        [7] (Just 7)

    testFindMaximum "Negative numbers"
        [-10,-3,-25,-1] (Just (-1))

    testFindMaximum "Duplicate maximum"
        [4,12,7,12,3] (Just 12)

    
task2 :: IO ()
task2 = do

    putStrLn "TASK 2: Reverse string"

    putStrLn ""
    runTask2Tests

    putStrLn ""
    putStrLn "Enter a word or sentence:"
    input <- getLine

    if null input
        then putStrLn "Input cannot be empty."
        else putStrLn ("Reversed text: " ++ reverseInput input)


task3 :: IO ()
task3 = do

    putStrLn ("TASK 3: Create a function that converts the first letter of" ++
        " each word in a sentence to an uppercase letter.")

    putStrLn ""
    runTask3Tests

    putStrLn ""
    putStrLn "Enter a sentence:"
    input <- getLine

    if null input
        then putStrLn "Input cannot be empty"
        else putStrLn ("Sentence with capitalized first letters: " 
                            ++ capitalizeInputSentence input)


task4 :: IO ()
task4 = do

    putStrLn ("TASK 4: Create a function that returns the second-to-last " ++
        "element from a list.")

    putStrLn ""
    runTask4Tests

    let numbers = [1, 2, 3, 4, 5]

    putStrLn ("List: " ++ show numbers)

    case secondLastElement numbers of        
        Just value ->
            putStrLn ("The second-to-last element is " ++ show value)
        Nothing ->
            putStrLn "The list does not contain enough elements"


task5 :: IO ()
task5 = do

    putStrLn ("TASK 5: Define an operator (!!) that returns the i-th " ++
        "element of a list.")

    putStrLn ""
    runTask5Tests

    let languages = 
            [ "Hasekell"
            , "C++"
            , "Python"
            , "Java"
            , "Scala"
            , "Rust"
            , "C#"
            , "JavaScript"
            ]

    putStrLn ""
    putStrLn ("Programming languages: " ++ show languages)
    putStrLn "Enter the element index:"

    input <- getLine

    case readMaybe input :: Maybe Int of
        
        Nothing ->
            putStrLn "Invalid input. Please enter a number"

        Just index ->
            if index < 0 || index >= length languages
                then putStrLn
                    ("Index out of bounds. Please enter number from 0 to "
                    ++ show (length languages -1))

                else putStrLn
                    ("Element at index " ++ show index ++ ": "
                    ++ (languages !! index))



task6 :: IO ()
task6 = do

    putStrLn ("TASK 6: Function that returns all possibilities for removing " ++
        "one element from a list")

    putStrLn ""
    runTask6Tests
    
    let numbers = [1, 2, 3, 4, 5]

    putStrLn ""
    putStrLn ("Original list: " ++ show numbers)
    putStrLn "Possible lists after removing one element: "

    printGaps 1 (gaps numbers)

printGaps :: Show a => Int -> [[a]] -> IO ()

printGaps _ [] = return ()
    
printGaps n (x : xs) = do
    putStrLn ("  " ++ show n ++ ". " ++ show x)
    printGaps (n + 1) xs



task7 :: IO ()
task7 = do
    
    putStrLn ("TASK 7: Create a function that inserts a value into a binary " ++
        "search tree (BST) and checks if the resulting tree is still a BST")

    runTask7Tests
    putStrLn ""
    
    let value = 7
    let newTree = insertTree value validTree

    putStrLn "Original tree: "
    print validTree

    putStrLn ""
    putStrLn ("Is the original tree a BST? "
        ++ show (isSearchTree validTree))
    
    putStrLn ""
    putStrLn ("Inserting value: " ++ show value)

    putStrLn "Tree after insertion: "
    print newTree

    putStrLn ""
    putStrLn ("Is the new tree a BST? "
        ++ show (isSearchTree newTree))

    putStrLn ""
    putStrLn ("Does insertion preserve the BST property? "
        ++ show (insertPreservesSearchTree value validTree))

    
    -- Incorrect implementation
    let wrongTree = insertTreeWrong value validTree

    putStrLn ""
    putStrLn ("Inserting value " ++ show value
        ++ " using the incorrect insertTreeWrong:")

    putStrLn ""
    print wrongTree

    putStrLn ""
    putStrLn ("Is the resulting tree a BST? "
        ++ show (isSearchTree wrongTree))

    putStrLn ""
    putStrLn ("Does insertTreeWrong preserve the BST property? "
        ++ show (insertWrongPreservesSearchTree value validTree))



task8 :: IO ()
task8 = do

    putStrLn "TASK 8: Prove the map and concat law."
    putStrLn ""

    runTask8Tests
    putStrLn ""

    let numbers = [[1, 2], [3, 4], [5]]
    let f = (*2)

    let leftResult = (map f . concat) numbers

    let rightResult = (concat . map (map f)) numbers

    putStrLn ("Original lists: " ++ show numbers)
    putStrLn "Function: multiply by 2"
    putStrLn ""

    putStrLn ("Left side result:  " ++ show leftResult)
    putStrLn ("Right side result: " ++ show rightResult)
    putStrLn ""

    putStrLn ("Are both sides equal? "
        ++ show (checkMapConcatLaw f numbers))



-- Galvenā interaktīvā izvēlne
showMenu :: IO ()
showMenu = do

    putStrLn ""
    putStrLn "FUNCTIONAL PROGRAMMING - LAB 1"
    putStrLn ""
    putStrLn "1. Find maximum number"
    putStrLn "2. Reverse a string"
    putStrLn "3. Capitalize sentence"
    putStrLn "4. Find second-to-last element"
    putStrLn "5. Custom list indexing"
    putStrLn "6. Generate list gaps"
    putStrLn "7. Binary search tree"
    putStrLn "8. Map and concat law"
    putStrLn ""
    putStrLn "0. Exit"
    putStrLn ""
    putStrLn "Select a task (0-8):"

    choice <- getLine

    case choice of

        "1" -> runTask task1
        "2" -> runTask task2
        "3" -> runTask task3
        "4" -> runTask task4
        "5" -> runTask task5
        "6" -> runTask task6
        "7" -> runTask task7
        "8" -> runTask task8

        "0" -> putStrLn "Exiting program. Goodbye!"

        _ -> do
            putStrLn "Invalid choice. Please enter a number from 0 to 8."
            showMenu


-- Uzdevuma palaišana
runTask :: IO () -> IO ()
runTask task = do

    putStrLn ""
    putStrLn ("--------------------------------------------------------------" ++
        "----------------------------------------------------------------")

    task

    putStrLn ("--------------------------------------------------------------" ++
        "----------------------------------------------------------------")
    putStrLn ""
    putStrLn "Press Enter to return to the main menu..."
    _ <- getLine

    showMenu



main :: IO ()
main = showMenu