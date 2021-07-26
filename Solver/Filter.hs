-- Módulo de funções de filtros de possibilidades

module Solver.Filter (filterKojun) where

import Data.List
import Solver.Utils

orthogonalFilter :: Int -> (Int, Int) -> [[[Int]]] -> Bool  -- Filtra elementos possíveis com base nos ortogonais adjacentes
orthogonalFilter value (i,j) matrix = not (aboveEquals || rightEquals || bottomEquals || leftEquals)
                                      where
                                        aboveEquals = i-1 >= 0
                                                      && length ((matrix!!(i-1))!!j) == 1
                                                      && ((matrix!!(i-1))!!j)!!0 == value
                                        rightEquals = j+1 < length matrix 
                                                      && length ((matrix!!i)!!(j+1)) == 1
                                                      && ((matrix!!i)!!(j+1))!!0 == value
                                        bottomEquals = i+1 < length matrix
                                                      && length ((matrix!!(i+1))!!j) == 1
                                                      && ((matrix!!(i+1))!!j)!!0 == value
                                        leftEquals = j-1 >= 0
                                                      && length ((matrix!!i)!!(j-1)) == 1
                                                      && ((matrix!!i)!!(j-1))!!0 == value

groupHasElemFilter :: Int -> (Int, Int) -> [[[Int]]] -> [[Int]] -> Bool -- Filtra pela regra de repetição de elementos
groupHasElemFilter value (i,j) matrix groups = notElem [value] ((getGroupsElemList matrix groups 0 (replicate (getNumberOfGroups groups 0) []))!!(groups!!i!!j))

upGreaterFilter :: Int -> (Int, Int) -> [[[Int]]] -> [[Int]] -> Bool    -- Filtra dado que o elemento de baixo precisa ser menor
upGreaterFilter value (i,j) matrix groups = (not bottomSameGroup) || value > head (matrix!!(i+1)!!j)
                                          where bottomSameGroup = (i+1) < length matrix && groups!!i!!j == groups!!(i+1)!!j

bottomLessFilter :: Int -> (Int, Int) -> [[[Int]]] -> [[Int]] -> Bool   -- Filtra dado que o elemento de cima precisa ser maior
bottomLessFilter value (i,j) matrix groups = (not topSameGroup) || value < last (matrix!!(i-1)!!j)
                                          where topSameGroup = i > 0 && groups!!i!!j == groups!!(i-1)!!j

filterKojunPosition :: [[[Int]]] -> [[Int]] -> (Int,Int) -> [Int]  -- Filtra posição da matriz de possibilidades
filterKojunPosition matrix groups (i,j) | length (matrix!!i!!j) == 1 = matrix!!i!!j
                                        | otherwise = filter (\x -> bottomLessFilter x (i,j) matrix groups) (
                                                        filter (\x -> upGreaterFilter x (i,j) matrix groups) (
                                                            filter (\x -> groupHasElemFilter x (i,j) matrix groups) (
                                                                filter (\x -> orthogonalFilter x (i,j) matrix) ((matrix!!i)!!j)
                                                            )
                                                        )
                                                      )
                                            -- Composição de funções de diferentes filtros

filterKojunLine :: [[[Int]]] -> [[Int]] -> (Int,Int) -> [[Int]]   -- Filtra uma linha da matriz de possibilidades
filterKojunLine matrix groups (i,j) | j < length matrix - 1 = filterKojunPosition matrix groups (i,j) : filterKojunLine matrix groups (i,j+1)
                                    | otherwise = [filterKojunPosition matrix groups (i,j)]

filterKojunLines :: [[[Int]]] -> [[Int]] -> Int -> [[[Int]]]   -- Filtra matriz de possibilidades
filterKojunLines matrix groups i | i < length matrix - 1 = filterKojunLine matrix groups (i,0) : filterKojunLines matrix groups (i+1)   -- Filtra próxima linha
                                 | otherwise = [filterKojunLine matrix groups (i,0)]

filterKojun :: [[[Int]]] -> [[Int]] -> [[[Int]]]   -- Filtra matriz de possibilidades enquanto é possível
filterKojun matrix groups | newMatrix == matrix = newMatrix
                          | otherwise = filterKojun newMatrix groups
                          where newMatrix = filterKojunLines matrix groups 0