# Functional-programming1

## Install Haskell

For Windows or macOS users, use GHCup to install Haskell. Follow instructions: https://www.haskell.org/ghcup/ 


## Haskell interactive shell (ghci)

https://www.haskell.org/ghcup/steps/#an-interactive-environment

Navigate (use e.g. PowerShell) to the directory where you have haskell files and start ghci with command `ghci`.


```
ghci
GHCi, version 9.2.8: https://www.haskell.org/ghc/  :? for help
ghci> 
```

Week-2 folder contains file listOfStrings.hs, that contains function headOrLast.

Load Haskell file using `:load` or `:l` command:
```
ghci> :l listOfStrings.hs
[1 of 1] Compiling Main             ( listOfStrings.hs, interpreted )
Ok, one module loaded.
```

After listOfStrings.hs file is loaded to ghci, use function headOrLast with correct arguments:
```
ghci> headOrLast ["hello"] 'h'
["hello"]
```

Use `:quit` or `:q` to exit ghci program: 
```
ghci> :q
Leaving GHCi.
```