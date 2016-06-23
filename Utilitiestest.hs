
module Main where

import Data.List
import Data.Matrix hiding ((!))
import Data.Tree
import Data.Maybe
import GameState
import ConnectCheck


--connect4Check :: Board -> Int -> Bool
--connect4Check board player = or $ map newAnd $ map ( map (==player)) (allsegments board)

allsegments :: Board -> [[Int]] 
allsegments board = allrows board ++ allcols board ++ allNegDia board ++ allPosDia board

allrows :: Board -> [[Int]]
allrows board = concat [rowsegments board i | i <-[1..6]] where
    rowsegments board i = [ [ getElem i j board | j<-[1..4] ]
                          , [ getElem i j board | j<-[2..5] ]
                          , [ getElem i j board | j<-[3..6] ]
                          , [ getElem i j board | j<-[4..7] ]
                          ]

allcols :: Board -> [[Int]]
allcols board = concat [colsegments board j | j <-[1..7]] where
    colsegments board j = [ [ getElem i j board | i<-[1..4] ]
                          , [ getElem i j board | i<-[2..5] ]
                          , [ getElem i j board | i<-[3..6] ]
                          ]

allNegDia :: Board -> [[Int]]
allNegDia board = filter (\x -> length x == 4) $ concat $ [ negDiaSegments board i 1 | i <-[2..3] ] ++ [ negDiaSegments board 1 j | j <-[1..4] ] where
    negDiaSegments board i j =  [ [getElem k l board | k <-[i..(i+3)], l <-[j..(j+3)], k<7, l<8, k - l == i - j]
                                , [getElem k l board | k <-[(i+1)..(i+4)], l <-[(j+1)..(j+4)], k<7, l<8, k - l == i - j]
                                , [getElem k l board | k <-[(i+2)..(i+5)], l <-[(j+2)..(j+5)], k<7, l<8, k - l == i - j]
                                ]

allPosDia :: Board -> [[Int]]
allPosDia board = filter (\x -> length x == 4) $ concat $ [ posDiaSegments board i 7 | i <-[1..3] ] ++ [ posDiaSegments board 1 j | j <-[4..6] ] where
    posDiaSegments board i j =  [ [getElem k l board | k <-[i..(i+3)], l <-[(j-3)..j], k<7, 0<l, k + l == i + j]
                                , [getElem k l board | k <-[(i+1)..(i+4)], l <-[(j-4)..(j-1)], k<7, 0<l, k + l == i + j]
                                , [getElem k l board | k <-[(i+2)..(i+5)], l <-[(j-5)..(j-2)], k<7, 0<l, k + l == i + j]
                                ]

dropInColumn :: Board -> Int -> Int -> Maybe (Board,Int,Int)
dropInColumn board k j = 
    let colj = getCol j board
        i =  (colCheck colj 5) + 1 :: Int
    in if 0<i then Just $ (setElem k (i,j) board, i, j) else Nothing

staticEval :: Int -> Int -> Board -> Int
staticEval i j board  = foldr1 (+) (map (scoreFour i j) $ allsegments board) where   
    scoreFour i j xs = case (indicator i xs, indicator j xs) of
            (0,0) -> 0
            (1,0) -> 1
            (2,0) -> 10
            (3,0) -> 100
            (4,0) -> 10000
            (0,1) -> (-1)
            (0,2) -> (-10)
            (0,3) -> (-100)
            (0,4) -> -10000
            (_,_) -> 0
    indicator a xs = foldr1 (+) (map (equality a) xs) 
    equality a b = if a == b then 1 ::Int else 0 ::Int


monteCarloTree :: Int -> Int -> (Board,Int) -> Tree (Board,Int)
monteCarloTree 0     player (board, num) = Node { rootLabel = (board, num), subForest = [] }
monteCarloTree depth player (board, num) = Node { rootLabel = (board, num), subForest = forest } where
        forest = pruneTerminal depth player children
        children = map fromJust $ filter (\x -> isJust x) [ dropInColumn board player j | j <-[1..7]]
        changePlayer x = case x of
            1 -> 2
            2 -> 1
            _ -> 0
        pruneTerminal depth player [] = []
        pruneTerminal depth player (x:xs) = if connect4 (first x) player (second x) (third x)
            then Node { rootLabel = ajust x, subForest = [] } : pruneTerminal depth player xs
            else monteCarloTree (depth-1) (changePlayer player) (ajust x) : pruneTerminal depth player xs
        first (x,y,z)  = x
        second (x,y,z) = y
        third (x,y,z)  = z
        ajust (x,y,z)  = (x,z)
        

eval :: Int -> Int -> Tree (Board,Int) -> Tree (Int,Int)
eval i j node = Node {rootLabel = (staticEval i j (fst $ rootLabel node), snd $ rootLabel node), subForest = map (eval j i) (subForest node) }

minimax :: Int -> Int -> [Tree (Int, Int)] -> [(Int,Int)]
minimax a b [t] = [(negate.fst $ bmx (-b) (-a) t, snd $ rootLabel t)]
minimax a b (t:ts) = if a' == b 
    then [(a', snd $ rootLabel t)] 
    else [(a', snd $ rootLabel t)] ++ minimax a' b ts where 
        a' = negate.fst $ bmx (-b) (-a) t

bmx :: Int -> Int -> Tree (Int, Int) -> (Int,Int)
bmx a b node = case subForest node of
    [] -> (a `max` (fst $ rootLabel node) `min` b, snd $ rootLabel node)
    _  -> cmx a b (subForest node)

cmx :: Int -> Int -> [Tree (Int, Int)] -> (Int,Int)
cmx a b [t] = (negate.fst $ bmx (-b) (-a) t, snd $ rootLabel t)
cmx a b (t:ts) = if a' == b 
    then (a', snd $ rootLabel t)
    else cmx a' b ts where 
        a' = negate.fst $ bmx (-b) (-a) t

bestScore :: (Int,Int) -> (Int,Int) -> Ordering
bestScore (a,b) (c,d) = case compare a c of
    GT -> GT
    LT -> LT
    EQ -> case compare b d of
        GT -> LT
        LT -> GT
        EQ -> EQ

bestMove :: Board -> Int -> [(Int,Int)]
bestMove board depth = minimax (-10000) 10000 forest where
    forest = subForest $ eval 2 1 (monteCarloTree depth 2 (board,1))



main :: IO()
main = do
    let zeroM = zero 6 7
    let test1 =  setElem 2 (5,6) . setElem 2 (5,4) . setElem 2 (5,3) . setElem 2 (6,2) . setElem 1 (6,4) . setElem 1 (6,3) . setElem 1 (6,6) . setElem 1 (6,7) $ zeroM
    let string = display $ prettyMatrix test1
    putStrLn string
    putStrLn $ show $ bestMove test1 7 
    return ()

