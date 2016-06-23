module RanAI where

import Control.Monad.State
import System.Random
import Data.Matrix hiding ((!))
import System.Exit
import GameState
import ConnectCheck


playRoundRanAI :: Game () 
playRoundRanAI = do
    command <- liftIO $ prompt ("Please choose a column Player 1")
    case words command of
        ["col", j_str] -> do
            state <- get
            let currM = gameBoard state
            let j = read j_str :: Int
            let columnj = getCol j currM
            let i = (colCheck columnj 5) + 1
            insertRanAI 1 i j currM 
            nstate <- get
            let newM = gameBoard nstate
            let string = display $ prettyMatrix newM
            liftIO $ putStrLn $ "\n" ++ string
            pvRanAiWin 1 i j
            aiAttack 
        ["help"] -> do
            liftIO $ putStrLn "Use the command 'col i' to put a piece in the ith column of the board. Type 'concede' to end the match if you think you cannot win."
            playRoundRanAI
        ["concede"] -> do
            liftIO $ putStrLn "The Computer Wins!!!"
        _ -> do
            liftIO $ putStrLn $ "Unrecognized command. Please try again"
            playRoundRanAI

aiAttack :: Game ()
aiAttack = do
    state <- get
    let currM = gameBoard state
    let gen = gameGen state
    let (j, ngen) = randomR (1,7) gen
    let columnj = getCol j currM
    let i = (colCheck columnj 5) + 1
    let newM = setElem 2 (i,j) currM
    modify $ \st -> st{ gameBoard = newM, gameGen = ngen}
    let string = display $ prettyMatrix newM
    liftIO $ putStrLn $ "The AI drops a piece in column " ++ show j ++ "."
    liftIO $ putStrLn $ string
    pvRanAiWin 2 i j

insertRanAI :: Int -> Int -> Int -> Board -> Game ()
insertRanAI k i j currM = if j >= 8 || j <= 0 
    then do
        liftIO $ putStrLn $ "The board has only 7 columns."
        playRoundRanAI
    else do
        case compare i 0 of
            GT -> do
                let newM = setElem k (i,j) currM
                modify $ \st -> st {gameBoard = newM}
            _ -> do
                liftIO $ putStrLn $ "The colum you selected is full."
                playRoundRanAI 

pvRanAiWin :: Int -> Int -> Int -> Game()
pvRanAiWin player i j = do
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
                    aiAttack
                2 -> do 
                    playRoundRanAI