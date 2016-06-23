module GameState where

import Control.Monad.State
import System.Random
import System.IO
import Data.Matrix hiding ((!))
import qualified Data.Vector as Vec

-- General Definitions of the State

type Board = Matrix Int

data GameState = GameState   
    { gameBoard :: Board
    , gameGen :: StdGen 
    }
    
type Game = StateT GameState IO 

prompt :: String -> IO String
prompt msg = do
    putStrLn msg
    hFlush stdout
    getLine

newBoard :: Board
newBoard = zero 6 7

colCheck :: Vec.Vector Int -> Int -> Int
colCheck _ (-1) = -1
colCheck vec i = case vec Vec.! i of 
    0 -> i
    _ -> colCheck vec (i-1)

display :: String -> String
display [] = []
display (x:xs) = case x of 
    '0' -> '*': display xs
    '1' -> 'O': display xs
    '2' -> 'X': display xs
    _   -> x: display xs