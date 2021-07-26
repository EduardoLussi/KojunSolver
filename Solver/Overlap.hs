-- Módulo de função de sobreposições das várias possibilidades

module Solver.Overlap (overlapMatrix) where

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