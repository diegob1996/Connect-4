module Player where

import Control.Monad.State
import Data.Matrix hiding ((!))
import System.Exit
import GameState
import ConnectCheck

playRoundP1 :: Game () 
playRoundP1 = do
    command <- liftIO $ prompt ("Please choose a column Player 1")
    case words command of
        ["col", j_str] -> do
            state <- get
            let currM = gameBoard state
            let j = read j_str :: Int
            let columnj = getCol j currM
            let i = (colCheck columnj 5) + 1
            playerInsert 1 i j currM
            nstate <- get
            let newM = gameBoard nstate
            let string = display $ prettyMatrix newM
            liftIO $ putStrLn $ "\n" ++ string
            pvpwin 1 i j
        ["help"] -> do
            liftIO $ putStrLn "Use the command 'col i' to put a piece in the ith column of the board. Type 'concede' to end the match if you think you cannot win."
            playRoundP1
        ["concede"] -> do
            liftIO $ putStrLn "Player 2 Wins!!!"
        _ -> do
            liftIO $ putStrLn $ "Unrecognized command. Please try again"
            playRoundP1

playRoundP2 :: Game () 
playRoundP2 = do
    command <- liftIO $ prompt ("Please choose a column Player 2")
    case words command of
        ["col", j_str] -> do
            state <- get
            let currM = gameBoard state
            let j = read j_str :: Int
            let columnj = getCol j currM
            let i = (colCheck columnj 5) + 1
            playerInsert 2 i j currM
            nstate <- get
            let newM = gameBoard nstate
            let string = display $ prettyMatrix newM
            liftIO $ putStrLn $ "\n" ++ string
            pvpwin 2 i j
        ["help"] -> do
            liftIO $ putStrLn "Use the command 'col i' to put a piece in the ith column of the board. Type 'concede' to end the match if you think you cannot win."
            playRoundP2
        ["concede"] -> do
            liftIO $ putStrLn "Player 1 Wins!!!"
        _ -> do
            liftIO $ putStrLn $ "Unrecognized command. Please try again"
            playRoundP2


playerInsert :: Int -> Int -> Int -> Board -> Game ()
playerInsert k i j currM = if j >= 8 || j <= 0 
    then do
        liftIO $ putStrLn $ "The board only has 7 columns."
        case k of
            1 -> do 
                playRoundP1
            2 -> do
                playRoundP2
    else do
        case compare i 0 of
            GT -> do
                let newM = setElem k (i,j) currM
                modify $ \st -> st {gameBoard = newM}
            _ -> do
                liftIO $ putStrLn $ "The colum you selected is full."
                case k of
                    1 -> do 
                        playRoundP1
                    2 -> do
                        playRoundP2    
            

pvpwin :: Int -> Int -> Int -> Game()
pvpwin player i j = do
    state <- get
    let currM = gameBoard state
    case connect4 currM player i j of
        True -> do
            liftIO $ putStrLn $ "Player " ++ show player ++ " Wins!!!"
            liftIO $ exitSuccess
        False -> do
            case player of 
                1 -> do
                    playRoundP2
                2 -> do 
                    playRoundP1
