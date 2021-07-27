-- Módulo principal da solução do jogo

module Solver.Solver (solve) where

import Solver.CheckSolution
import Solver.Overlap
import Solver.Filter
import Solver.Colapse
import Solver.Utils

findSolution :: [[Int]] -> [[Int]] -> [[[Int]]] -> Int -> [[Int]]
findSolution matrix groups overlapedMatrix i | checkSolution currentSolution groups = currentSolution
                                             | otherwise = findSolution matrix groups overlapedMatrix (i+1)
                                             where currentSolution = colapseMatrix overlapedMatrix i

solve :: [[Int]] -> [[Int]] -> [[Int]]  -- Soluciona Kojun
solve matrix groups = findSolution matrix groups (filterKojun (overlapMatrix matrix groups (getGroupSizes groups) 0) groups) 0
