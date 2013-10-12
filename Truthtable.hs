import Control.Monad
import Language.Haskell.Interpreter
import Data.List

type TruthTable = [[Bool]]
type TruthTableResult = [([Bool], Bool)]

-- Create an list of true value in a row
-- this function contruct the list from last column and append it to the previos column and reverse the result
-- Input: current row, current column,  how many rows for t's pair or f's pair 
-- Output: True values in a row
rowValues :: Integer -> Integer -> Integer-> [Bool]
rowValues i j n = reverse (rowValues' i j n)
                where rowValues' _ (-1) _ = []
                      rowValues' i j n = let previousColumns = rowValues' i (j-1) (n*2)
                                             truthValue      =  even (div i n)
                                         in  truthValue : previousColumns
                  
-- Build a truth table with n variables
-- Input: the number of variable
-- Output: An Array of an array of Bool
truthTable :: Integer -> TruthTable
truthTable n = let lastRowIndex = 2^n-1
                   lastColumnIndex = n-1
               in  map (\x -> rowValues x (lastColumnIndex) 1) [0..lastRowIndex]

-- Apply the function to each row of a truth table, and return the result 
-- Input : function going to be applied, number of variable in the truth table
-- Output: The results of the function on each row of the truth table
solveTable :: ([Bool] -> Bool) -> Integer -> (TruthTableResult)
solveTable f n = let table = truthTable n
                     result = map f table
                 in  zipWith (\v r -> (v, r)) table result

-- Contruct the string of truth table
-- Input: a truthtable with combination of variable value
-- Output: a string sepearte each row of values
showRows :: TruthTable -> String
showRows [] = ""
showRows (x:xs) = (show x) ++ "\n" ++ (showRows xs)

-- Show truth table in pretty format
showTable :: TruthTable -> IO ()
showTable t = let s = showRows t
              in putStrLn s

-- Show truth table result in pretty format
showTableResult :: TruthTableResult -> IO ()
showTableResult t = putStrLn (showTableResult' t)
                where rowStr str (v, r) = str ++ ((show v) ++ ",\t" ++ (show r) ++ "\n")                     
                      showTableResult' t = foldl rowStr "" t


-- TO BE DONE LATER, SOME PROBLEMS EXIST
-- instance Show TruthTable where
--     show [] = ""
--     show x = showRows x
--


main = do
    putStrLn "Please enter the numbers of proposition variable"
    numStr <- getLine
    let num = read numStr :: Integer

    putStrLn "Please enter your formula"
    putStrLn "eg: let f (x:y:_) = x && y"
    fExpr <- getLine
    let exp = fExpr ++ " in f"

    -- Create an interpreter that runs exp
    r <- runInterpreter $ do
            setImports ["Prelude"]
            interpret exp (const True :: [Bool] -> Bool)
    -- run it and get an interface to the function
    case r of
        Left err -> putStrLn $ "Parsing error, please input the formula again"
        Right f  -> do
            showTableResult (solveTable f num)

