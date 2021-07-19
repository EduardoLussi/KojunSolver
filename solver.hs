module Solver (solve) where

import Data.List               

countElemMatrix :: [[Int]] -> Int -> Int    -- Conta ocorrências de um elemento em uma matriz
countElemMatrix [] _ = 0
countElemMatrix (h:t) elem = length (filter (==elem) h) + countElemMatrix t elem

countGroups :: [[Int]] -> Int -> [Int]  -- Conta o tamanho de um grupo
countGroups groups group | nextGroupOcurrences == 0 = [countElemMatrix groups group]
                         | otherwise = countElemMatrix groups group : countGroups groups (group+1)
                         where nextGroupOcurrences = countElemMatrix groups (group+1)

getGroupSizes :: [[Int]] -> [Int]   -- Retorna uma lista com o tamanho de cada grupo
getGroupSizes groups = countGroups groups 0

-- FUNÇÕES DE FILTRO =========================================================

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

filterKojunPosition :: [[[Int]]] -> (Int,Int) -> [Int]  -- Filtra posição da matriz de possibilidades
filterKojunPosition matrix (i,j) = filter (\x -> orthogonalFilter x (i,j) matrix) ((matrix!!i)!!j)
                                            -- Composição de funções de diferentes filtros
                                            -- Devem ser adicionados mais filtros

filterKojunLine :: [[[Int]]] -> (Int,Int) -> [[Int]]   -- Filtra uma linha da matriz de possibilidades
filterKojunLine matrix (i,j) | j < length matrix - 1 = filterKojunPosition matrix (i,j) : filterKojunLine matrix (i,j+1)
                             | otherwise = [filterKojunPosition matrix (i,j)]

filterKojunLines :: [[[Int]]] -> Int -> [[[Int]]]   -- Filtra matriz de possibilidades
filterKojunLines matrix i | i < length matrix - 1 = filterKojunLine matrix (i,0) : filterKojunLines matrix (i+1)   -- Filtra próxima linha
                     | otherwise = [filterKojunLine matrix (i,0)]

filterKojun :: [[[Int]]] -> [[[Int]]]   -- Filtra matriz de possibilidades enquanto é possível
filterKojun matrix | newMatrix == matrix = newMatrix
                   | otherwise = filterKojun newMatrix
                   where newMatrix = filterKojunLines matrix 0

-- FUNÇÕES DE SOBREPOSIÇÃO ===================================================

overlapLine :: [Int] -> [[Int]] -> [Int] -> (Int, Int) -> [[Int]] -- Sobrepõe as possibilidades em cada posição da linha
overlapLine (h:t) groups groupSizes (i,j) =
    if length t == 0 then -- Última linha
        if h == 0 then -- Elemento não definido
            [[1..groupSizes!!((groups!!i)!!j)]] -- Preenche posição com lista de 1 até tamanho do grupo
        else
            [[h]]   -- Elemento já definido
    else
        if h == 0 then
            [1..groupSizes!!((groups!!i)!!j)] : overlapLine t groups groupSizes (i,j+1) -- Concatena com sobreposição das próximas posições
        else 
            [h] : overlapLine t groups groupSizes (i,j+1)

overlapMatrix :: [[Int]] -> [[Int]] -> [Int] -> Int -> [[[Int]]]   -- Sobrepõe os valores possíveis em cada linha "i"
overlapMatrix (h:t) groups groupSizes i | length t == 0 = [overlapLine h groups groupSizes (i,0)]
                                        | otherwise = overlapLine h groups groupSizes (i,0) : overlapMatrix t groups groupSizes (i+1)
                                                                                            -- Concatena com sobreposição das próximas linhas

-- FUNÇÕES DE COLAPSO ========================================================

colapseLine :: [[Int]] -> Int -> [Int]  -- Colapsa linha na possibilidade i
colapseLine (h:t) i | length t == 0 = [h!!(mod i (length h))]   -- Possibilidade da posição é i mod (número de possibilidades da posição)
                    | otherwise = h!!(mod i (length h)) : colapseLine t (div i (length h))  -- Colapso i da próxima posição é divisão inteira 
                                                                                            -- de i por (número de possibilidades da posição)

getNextColapseIndex :: [[Int]] -> Int -> Int    -- Retorna índice do primeiro colapso da próxima linha
getNextColapseIndex (h:t) i | length t == 0 = div i (length h)
                            | otherwise = getNextColapseIndex t (div i (length h))

colapseMatrix :: [[[Int]]] -> Int -> [[Int]]    -- Colapsa a matriz em uma possibilidade "i"
colapseMatrix (h:t) i | length t == 0 = [colapseLine h i]
                      | otherwise = colapseLine h i : colapseMatrix t (getNextColapseIndex h i)

-- FUNÇÃO PRINCIPAL =========================================================

solve :: [[Int]] -> [[Int]] -> [[[Int]]]  -- Soluciona Kojum
--solve matrix groups = colapseMatrix (filterKojun (overlapMatrix matrix groups (getGroupSizes groups) 0)) 9999999999999999999999999999999 -- Número da possibilidade
solve matrix groups = filterKojun (overlapMatrix matrix groups (getGroupSizes groups) 0)
