module Main (main) where

-- import Parse -- parse do arquivo de input
import System.IO -- args do usuário (caso precisar)
import Data.List.Split -- split de strings
import Parse -- fazer parse do arquivo de input e montar Kojun
import Solver -- Solucionador


main = do
    let file = "input1.txt"
    str <- readFile file
    let stringList = splitOn "\n" str

    let size = stringList !! 0
    let sizeNumber = read size :: Int

    let listNumber = parseFile (tail stringList) sizeNumber
    
    let kojun = firstNelements listNumber sizeNumber
    let grupos = lastNelements listNumber sizeNumber

    let tuplasGrupos = getGroupTuples grupos 
    let jogoResolvido = solve kojun grupos

    print (tuplasGrupos)

    -- print(kojun)
    -- print(grupos)
    -- print(solve kojun grupos)

    -- let solucao = solve kojun grupos
    -- print (solucao)
    -- print(solucao!!0)
    -- print(solucao!!0!!0)

    -- print (filter isJust (tuplasGrupos!!0)!!2)