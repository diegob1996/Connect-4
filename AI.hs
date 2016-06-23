module AI where

import Control.Monad.State
import Data.Matrix hiding ((!))
import System.Exit
import ConnectCheck
import GameState
import AIUtilities

playRoundAI :: Int -> Game () 
playRoundAI dlevel = do
    command <- liftIO $ prompt ("Please choose a column Player 1")
    case words command of
        ["col", j_str] -> do
            state <- get
            let currM = gameBoard state
            let j = read j_str :: Int
            let columnj = getCol j currM
            let i = (colCheck columnj 5) + 1
            insertAI dlevel 1 i j currM 
            nstate <- get
            let newM = gameBoard nstate
            let string = display $ prettyMatrix newM
            liftIO $ putStrLn $ "\n" ++ string
            pvaiwin dlevel 1 i j
            aiAttack dlevel
        ["help"] -> do
            liftIO $ putStrLn "Use the command 'col i' to put a piece in the ith column of the board. Type 'concede' to end the match if you think you cannot win."
            playRoundAI dlevel
        ["concede"] -> do
            liftIO $ putStrLn "The Computer Wins!!!"
        _ -> do
            liftIO $ putStrLn $ "Unrecognized command. Please try again"
            playRoundAI dlevel

aiAttack :: Int -> Game ()
aiAttack dlevel = do
    state <- get
    let currM = gameBoard state
    let j = bestMove currM dlevel 
    let columnj = getCol j currM
    let i = (colCheck columnj 5) + 1
    let newM = setElem 2 (i,j) currM
    modify $ \st -> st{ gameBoard = newM}
    let string = display $ prettyMatrix newM
    liftIO $ putStrLn $ "The AI drops a piece in column " ++ show j ++ "."
    liftIO $ putStrLn $ string
    pvaiwin dlevel 2 i j

insertAI :: Int -> Int -> Int -> Int -> Board -> Game ()
insertAI dlevel k i j currM = if j >= 8 || j <= 0 
    then do
        liftIO $ putStrLn $ "The board has only 7 columns."
        playRoundAI dlevel
    else do
        case compare i 0 of
            GT -> do
                let newM = setElem k (i,j) currM
                modify $ \st -> st {gameBoard = newM}
            _ -> do
                liftIO $ putStrLn $ "The colum you selected is full."
                playRoundAI dlevel

pvaiwin :: Int -> Int -> Int -> Int -> Game()
pvaiwin dlevel player i j = do
    state <- get
    let currM = gameBoard state
    case connect4 currM player i j of
        True -> case player of
            1 -> do
                liftIO $ putStrLn $ "Player Wins!!!"
                liftIO $ exitSuccess
            2 -> do 
                liftIO $ putStrLn $ "The Computer Wins!!!"
                liftIO $ exitSuccess
        False -> do
            case player of 
                1 -> do
                    aiAttack dlevel
                2 -> do 
                    playRoundAI dlevel