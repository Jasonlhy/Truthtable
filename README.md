#Truthtable

It's a small library to build a truth table. It can apply a function on the truth table, and can get the results. It was created for verifying the answers in my discrete maths exercise.

#Install the dependencies
You need to install the ghci and the hint package.

1. Download the ghci from http://www.haskell.org/platform/
2. After you install the ghci, install the hint package using command: cabal install hint

#Usage
With the source code, you can compile it into executable. You can also either use the codes in ghci or run the file directlys.


###Compile in executable and run it

    ghc --make Truthtable.hs
    ./Truthtable
    Please enter the numbers of proposition variable
    2
    Please enter your formula
    eg: let f (x:y:_) = x && y
    
###Play in ghci

    Prelude> :l Truthtable.hs 
    [1 of 1] Compiling Main             ( Truthtable.hs, interpreted )
    Ok, modules loaded: Main.
    
###Run it directly

    runhaskell Truthtable.hs 
    Please enter the numbers of proposition variable
    2
    Please enter your formula
    eg: let f (x:y:_) = x && y

#Functions
The executable file just provide one function right now, solving truth table with a function. However, you can do more tasks in the ghci.

###Build a truth table
Suupose we want to build a truth table like this, which has 2 variables:

| x        | y       | 
| -------- |:--------| 
| True     | True    | 
| True     | False   | 
| False    | True    | 
| False    | False   | 

    *Main> truthTable 2
    [[True,True],[True,False],[False,True],[False,False]]
    *Main> showTable (truthTable 2)
    [True,True]
    [True,False]
    [False,True]
    [False,False]

###Apply a function on it
Let create a hand-made **and** function, and apply it to our truth table.

f(x, y) = x && y

| x        | y       | x && y |
| -------- |:--------|--------|
| True     | True    | True   | 
| True     | False   | False  |
| False    | True    | False  |
| False    | False   | False  |


We have to create our own function and pass it to the library function, **solveTable** for the calculation. Also, we need to specify the number of variables in the truth table.

    *Main> let f (x:y:_) = x && y
    *Main> solveTable f 2
    [([True,True],True),([True,False],False),([False,True],False),([False,False],False)]
    *Main> showTableResult (solveTable f 2)
    [True,True], True
    [True,False], False
    [False,True], False
    [False,False], False


The output will be an array of tuples, each tuple consist of the truth values applying on the function and the output of the function.

#Logical operation
When you need to define the function in the program, you need use the following syntax to represent the logical operation.

| Logical operation      | Syntax           | 
| -----------------------|:-----------------|
| a AND b                | a && b           |
| a OR b                 | a || b           | 
| a IMPLIES b            | a <= b           |
| NOT a                  | not a            |
