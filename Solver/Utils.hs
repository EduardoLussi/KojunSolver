-- Módulo com funções gerais úteis

module Solver.Utils (getGroupsElemList, getGroupSizes, getNumberOfGroups) where

countElemMatrix :: Eq a => [[a]] -> a -> Int    -- Conta ocorrências de um elemento em uma matriz
countElemMatrix [] _ = 0
countElemMatrix (h:t) elem = length (filter (==elem) h) + countElemMatrix t elem

countGroups :: [[Int]] -> Int -> [Int]  -- Conta o tamanho de um grupo
countGroups groups group | nextGroupOcurrences == 0 = [countElemMatrix groups group]
                         | otherwise = countElemMatrix groups group : countGroups groups (group+1)
                         where nextGroupOcurrences = countElemMatrix groups (group+1)

getGroupSizes :: [[Int]] -> [Int]   -- Retorna uma lista com o tamanho de cada grupo
getGroupSizes groups = countGroups groups 0

getNumberOfGroups :: [[Int]] -> Int -> Int
getNumberOfGroups groups group | countElemMatrix groups group == 0 = group
                               | otherwise = getNumberOfGroups groups (group+1)

getGroupsElemListLine :: [[a]] -> [[Int]] -> (Int, Int) -> [[a]] -> [[a]]    -- Auxiliar de getGroupsElemList para varredura dos elementos de cada linha
getGroupsElemListLine matrix groups (i, j) groupsElemList 
    | (j+1) == length matrix = (
                                (take ((groups!!i)!!j) groupsElemList) ++ 
                                [groupsElemList!!((groups!!i)!!j) ++ [(matrix!!i)!!j]] ++ 
                                (drop (((groups!!i)!!j)+1) groupsElemList)
                               )
    | otherwise = getGroupsElemListLine matrix groups (i, j+1) (
                                                                (take ((groups!!i)!!j) groupsElemList) ++ 
                                                                [groupsElemList!!((groups!!i)!!j) ++ [(matrix!!i)!!j]] ++ 
                                                                (drop (((groups!!i)!!j)+1) groupsElemList)
                                                               )

getGroupsElemList :: [[a]] -> [[Int]] -> Int -> [[a]] -> [[a]]   -- Cria lista de elementos de cada grupo
getGroupsElemList matrix groups i groupsElemList 
    | (i+1) == length matrix = getGroupsElemListLine matrix groups (i, 0) groupsElemList
    | otherwise = getGroupsElemList matrix groups (i+1) (getGroupsElemListLine matrix groups (i, 0) groupsElemList)