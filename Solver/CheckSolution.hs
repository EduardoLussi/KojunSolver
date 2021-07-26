-- Módulo referente à verificação de uma solução

module Solver.CheckSolution (checkSolution) where

import Data.List
import Solver.Utils

checkRepetitionGroups :: [[Int]] -> Bool    -- Auxiliar de checkRepetition, verifica se lista de elementos de cada grupo é da forma [1..(length listaGrupo)]
checkRepetitionGroups [] = True
checkRepetitionGroups (h:t) | sort h /= [1..(length h)] = False
                            | otherwise = checkRepetitionGroups t

checkRepetition :: [[Int]] -> [[Int]] -> Bool   -- Verifica se cada grupo possui elementos repetidos
checkRepetition matrix groups = checkRepetitionGroups (getGroupsElemList matrix groups 0 (replicate (getNumberOfGroups groups 0) []))

checkAdjacentsLine :: [[Int]] -> [[Int]] -> (Int, Int) -> Bool  -- Auxiliar de checkAdjacents, percorre as linhas
checkAdjacentsLine matrix groups (i, j) | (j+1) == length matrix = not (rightEquals || bottomEquals)
                                        | rightEquals || bottomEquals = False
                                        | otherwise = checkAdjacentsLine matrix groups (i, j+1)
                                        where rightEquals = j+1 < length matrix && (matrix!!i)!!(j+1) == (matrix!!i)!!j
                                              bottomEquals = i+1 < length matrix && (matrix!!(i+1))!!j == (matrix!!i)!!j

checkAdjacents :: [[Int]] -> [[Int]] -> Int -> Bool    -- Verifica se cada elemento da solução possui valores adjacentes iguais a ele
checkAdjacents matrix groups i | (i+1) == length matrix = checkAdjacentsLine matrix groups (i, 0)
                               | otherwise = checkAdjacentsLine matrix groups (i, 0) && checkAdjacents matrix groups (i+1)

checkBottomLessLine :: [[Int]] -> [[Int]] -> (Int, Int) -> Bool -- Auxiliar de checkBottomLess, percorre as linhas
checkBottomLessLine matrix groups (i, j) | (j+1) == length matrix = (not bottomSameGroup) || bottomLess
                                         | bottomSameGroup && (not bottomLess) = False
                                         | otherwise = checkBottomLessLine matrix groups (i, j+1)
                                         where bottomSameGroup | (i+1) == length matrix = False
                                                               | otherwise = (groups!!(i+1))!!j == (groups!!i)!!j
                                               bottomLess = (matrix!!(i+1))!!j < (matrix!!i)!!j

checkBottomLess :: [[Int]] -> [[Int]] -> Int -> Bool    -- Verifica se células inferiores possuem valores menores se pertencentes ao mesmo grupo
checkBottomLess matrix groups i | (i+1) == length matrix = checkBottomLessLine matrix groups (i, 0)
                                | otherwise = checkBottomLessLine matrix groups (i, 0) && checkBottomLess matrix groups (i+1)

checkSolution :: [[Int]] -> [[Int]] -> Bool -- Verifica se matriz é solução
checkSolution matrix groups = checkAdjacents matrix groups 0 && checkBottomLess matrix groups 0 && checkRepetition matrix groups