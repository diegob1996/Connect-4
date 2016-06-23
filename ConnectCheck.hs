module ConnectCheck where

import Data.Matrix hiding ((!))
import GameState

newAnd :: [Bool] -> Bool
newAnd x = case x of
    [] -> False
    _ ->    if length x == 4 
            then
                and x
            else
                False

connect4 :: Board -> Int -> Int -> Int -> Bool
connect4 board player i j = or $ [connectRow board player i j, connectCol board player i j, connectDia board player i j, connectTran board player i j]

connectRow :: Board -> Int -> Int -> Int -> Bool
connectRow board player i j =  or $ 
    [ newAnd $ map (==player) [getElem  i k board | k <- [(j-3)..j], 0<k, k<8]
    , newAnd $ map (==player) [getElem  i k board | k <- [(j-2)..(j+1)], 0<k, k<8]
    , newAnd $ map (==player) [getElem  i k board | k <- [(j-1)..(j+2)], 0<k, k<8]
    , newAnd $ map (==player) [getElem  i k board | k <- [j..(j+3)], 0<k, k<8]
    ]

connectCol :: Board -> Int -> Int -> Int -> Bool
connectCol board player i j =  or $ 
    [ newAnd $ map (==player) [getElem k j board | k <- [(i-3)..i], 0<k, k<7]
    , newAnd $ map (==player) [getElem k j board | k <- [(i-2)..(i+1)], 0<k, k<7]
    , newAnd $ map (==player) [getElem k j board | k <- [(i-1)..(i+2)], 0<k, k<7]
    , newAnd $ map (==player) [getElem k j board | k <- [i..(i+3)], 0<k, k<7]
    ]

connectDia :: Board -> Int -> Int -> Int -> Bool
connectDia board player i j = or $ 
    [ newAnd $ map (==player) [getElem k l board | k <- [(i-3)..i], 0<k, k<7, l <- [(j-3)..j], 0<l, l<8, k - l == i - j]
    , newAnd $ map (==player) [getElem k l board | k <- [(i-2)..(i+1)], 0<k, k<7, l <- [(j-2)..(j+1)], 0<l, l<8, k - l == i - j]
    , newAnd $ map (==player) [getElem k l board | k <- [(i-1)..(i+2)], 0<k, k<7, l <- [(j-1)..(j+2)], 0<l, l<8, k - l == i - j]
    , newAnd $ map (==player) [getElem k l board | k <- [i..(i+3)], 0<k, k<7, l <- [j..(j+3)], 0<l, l<8, k - l == i - j]
    ]

connectTran :: Board -> Int -> Int -> Int -> Bool
connectTran board player i j = or $ 
    [ newAnd $ map (==player) [getElem k l board | k <- [(i-3)..i], 0<k, k<7, l <- [j..(j+3)], 0<l, l<8, k + l == i + j]
    , newAnd $ map (==player) [getElem k l board | k <- [(i-2)..(i+1)], 0<k, k<7, l <- [(j-1)..(j+2)], 0<l, l<8, k + l == i + j]
    , newAnd $ map (==player) [getElem k l board | k <- [(i-1)..(i+2)], 0<k, k<7, l <- [(j-2)..(j+1)], 0<l, l<8, k + l == i + j]
    , newAnd $ map (==player) [getElem k l board | k <- [i..(i+3)], 0<k, k<7, l <- [(j-3)..j], 0<l, l<8, k + l == i + j]
    ]