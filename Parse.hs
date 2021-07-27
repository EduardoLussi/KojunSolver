-- Módulo que monta as estruturas de dados

module Parse (parseFile, firstNelements, lastNelements, printMatrix) where

import Data.List.Split -- split de strings
import Data.List

-- pegar informações do arquivo txt
parseFile :: [String] -> Int -> [[Int]]
parseFile [] _ = []
parseFile lista size = convertToMatrix lista size (size * 2)

convertToMatrix :: [String] -> Int -> Int -> [[Int]]
convertToMatrix [] _ _ = []
convertToMatrix (head:tale) size n = ((convertToInt (splitOn " " head)) : (convertToMatrix tale size (n - 1)))

convertToInt :: [String] -> [Int]
convertToInt list = map (read :: String -> Int) list

firstNelements :: [[Int]] -> Int -> [[Int]]
firstNelements [] _ = []
firstNelements (head:tale) n | n > 0 = head : (firstNelements tale (n - 1))
                             | otherwise = []

lastNelements :: [[Int]] -> Int -> [[Int]]
lastNelements [] _ = []
lastNelements lista n = reverse ((firstNelements (reverse lista) n))

-- imprimir de uma forma agradável para o usuário a solução da matriz
takeEachRow :: [Int] -> String
takeEachRow [] = ""
takeEachRow (head:tale) = (show head) ++ " " ++ takeEachRow tale

printMatrix :: [[Int]] -> String
printMatrix [] = ""
printMatrix (head:tale) = takeEachRow head ++ "\n" ++ printMatrix tale