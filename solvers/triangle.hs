module Triangle  where 

import Search 
import System.Random.Shuffle  (shuffleM)
import Data.Maybe (listToMaybe,mapMaybe)
import Data.List (nub)

--build out possible moves 
tlist = [ [0,1,3],
    [0,2,5],
    [1,3,6],
    [1,4,8],
    [2,4,7],
    [2,5,9],
    [3,6,10],
    [3,7,12 ],
    [4,7,11 ],
    [4,8,13 ],
    [5,8,12],
    [5,9,14],
    [3,4,5],
    [6,7,8],
    [7,8,9],
    [10,11,12],
    [11,12,13],
    [12,13,14 ]
    ] :: [[Int]]

transitions = tlist ++ ( map reverse tlist ) 

-- 
getMove :: [[Bool]] -> [Int]-> Maybe [Int]
getMove games [x,y,z] | (game !! x) && (game !! y) && not (game !! z) = Just [x,y,z]
                    | not  (game !! x) && (game !! y) && (game !! z) = Just [x,y,z]
                    | otherwise = Nothing
                    where 
                        game = head games 

takeMove  :: [[Bool]] -> [Int] -> [[Bool]]
takeMove games [x,y,z] = let 
    flip ind value [x,y,z ] | ind == x = not value 
                            | ind == y = not value 
                            | ind == z = not value 
                            | otherwise = value 
    in  [[flip a b [x,y,z] | (a,b) <- zip [0..] game  ]] ++ games 
    where game = head games 

step :: [[Bool]] -> [[[Bool]]]
step game = let 
    moves = mapMaybe (getMove game) transitions 
    in nub $ map (takeMove game) moves
    

isSolved :: [[Bool]] -> Bool 
isSolved game = ( sum $ map fromEnum (head game)) == 1 


-- getSolution :: Maybe a -> a
getSolution (Just x) = x 


main :: IO ()
main = let 
    basegame = (take 14 $ repeat True) ++ [False] 
    in do 
        g <- shuffleM basegame
        let game = [ g ]
        
        let pegGame = SearchProb game expand isDone where 
                start = game
                expand = step 
                isDone = isSolved 

        let solution = reverse ( getSolution $ dfs pegGame) 
        print solution
