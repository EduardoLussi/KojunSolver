module Main (main) where

-- import Parse -- parse do arquivo de input
import System.IO -- args do usuário (caso precisar)
import Data.List.Split -- split de strings
import Parse -- fazer parse do arquivo de input e montar Kojun
import Solver -- Solucionador

main = do
    let file = "input.txt"
    str <- readFile file
    let stringList = splitOn "\n" str

    let size = stringList !! 0
    let sizeNumber = read size :: Int

    let listNumber = parseFile (tail stringList) sizeNumber
    
    let kojum = firstNelements listNumber sizeNumber
    let grupos = lastNelements listNumber sizeNumber

    print(kojum)
    print(grupos)
    print(solve kojum grupos)