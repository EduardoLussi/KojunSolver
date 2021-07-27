-- Módulo principal

module Main (main) where

import System.IO -- args do usuário (caso precisar)
import Data.List.Split -- split de strings
import Parse -- fazer parse do arquivo de input e montar Kojun
import Solver.Solver -- Solucionador


main = do
    let file = "inputs/hard.txt"    -- NOME DO ARQUIVO
    str <- readFile file
    let stringList = splitOn "\n" str

    let size = stringList !! 0
    let sizeNumber = read size :: Int

    let listNumber = parseFile (tail stringList) sizeNumber
    
    let kojun = firstNelements listNumber sizeNumber
    let grupos = lastNelements listNumber sizeNumber

    putStrLn (printMatrix (solve kojun grupos))
