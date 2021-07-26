-- Módulo de funções de colapso em uma possibilidade

module Solver.Colapse (colapseMatrix) where

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