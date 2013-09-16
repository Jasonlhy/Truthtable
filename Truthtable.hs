import Data.List

type TruthTable = [[Bool]]

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
solveTable :: ([Bool] -> Bool) -> Integer ->  [Bool]
solveTable f n = let table = truthTable n
                 in map f table
