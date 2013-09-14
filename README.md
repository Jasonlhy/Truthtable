#Truthtable

It's a small library to build a truth table. It can apply the function on the truth table and gets the results. It was created for verifying the answers in my discrete maths exercise.

#Usage
Load the source code into the ghci

    Prelude> :l Truthtable.hs 
    [1 of 1] Compiling Main             ( Truthtable.hs, interpreted )
    Ok, modules loaded: Main.

#Build a truth table
Suupose we want to build a truth table like this, which have 2 variables:

| x        | y       | 
| -------- |:--------| 
| True     | True    | 
| True     | False   | 
| False    | True    | 
| False    | False   | 

    *Main> truthTable 2
    [[True,True],[True,False],[False,True],[False,False]]

#Apply a function on it
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
    [True,False,False,False]

The output will be an array of boolean values which are the results of the function applied on each row of the truth table.
