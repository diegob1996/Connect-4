module Main where

import Control.Monad.State
import System.Random
import System.Exit
import GameState
import Player
import RanAI
import AI

main :: IO () --the, suprisingly easy, main loop
main = do
    gen <- getStdGen
    let initialState = GameState {gameBoard = newBoard, gameGen = gen}
    _ <- runStateT startGame initialState
    return ()

startGame :: Game ()
startGame = do
    command <- liftIO $ prompt "Type \"help\" for help, \"play\" to start the game or \"quit\" to end the game."
    case command of
        "play" -> do
            gametype <- liftIO $ prompt "Do you want to play PVP or Random AI, Easy AI, Medium AI, Hard AI?"
            case gametype of
                
                "Random AI"   -> playRoundRanAI
                "PVP"         -> playRoundP1
                "Easy AI"     -> playRoundAI 3
                "Medium AI"   -> playRoundAI 5
                "Hard AI"     -> playRoundAI 7

                _ -> do
                    liftIO $ putStrLn $ "Unrecognized command. Please try again"
                    startGame
        "help" -> do
            liftIO $ putStrLn "Use the command 'col i' to put a piece in the ith column of the board. Type 'concede' to end the match if you think you cannot win. You can also type 'help' in-game to see this message again."
            startGame
        "quit" -> do
            liftIO $ exitSuccess
        _  -> do
            liftIO $ putStrLn "This is not a valid command"
            startGame