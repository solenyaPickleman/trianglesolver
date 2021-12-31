module Search where

import Data.List (find,nub)
import Data.Maybe (listToMaybe,mapMaybe)
import Debug.Trace (trace)

-- data definitions 
data SearchProb a = SearchProb { start :: a , expand :: a -> [a], isDone :: a -> Bool }
type SearchAlgo a = SearchProb a -> Maybe a

--Depth First search  
dfs :: SearchAlgo a
dfs (SearchProb start expand isDone) = loop start where
  loop x | isDone x  = Just x
         | otherwise = listToMaybe $ mapMaybe loop (expand x)

-- --Breadth first search 
-- bfs :: SearchAlgo a
-- bfs (SearchProb start expand isDone) = loop [start] where
--   loop xs | any isDone xs = find isDone xs
--           | otherwise     = loop (concatMap expand xs)

-- --Deduplicate before BFS map 
-- slightlyBetterBfs :: Eq a=> SearchAlgo a
-- slightlyBetterBfs (SearchProb start expand isDone) = loop [start] where
--   loop xs | any isDone xs = find isDone xs
--           | otherwise     = loop (nub $ concatMap expand xs)
