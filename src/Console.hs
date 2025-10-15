module Console (
    consoleRead,
    processRead
) where

import Parse(parseRead)
import Config(defaultConf, Config)

processRead :: [String] -> IO() 
processRead [] = do 
    putStrLn "lol where? Ok using defaults"
    startInteractive defaultConf
processRead args = do
    startInteractive $ parseRead args defaultConf

startInteractive :: Config -> IO()
startInteractive conf = do
    putStrLn $ "Current config is " <> show conf
    putStrLn "Type EOF if you want to exit"
    consoleRead conf

consoleRead :: Config -> IO()
consoleRead conf = do
    putStr "> "
    input <- getLine
    case input of
        "EOF" -> finishComputation
        _ -> do
            let result = processComputation conf input
            putStrLn $ "Result: " ++ show result
            consoleRead conf

finishComputation :: IO ()
finishComputation = putStrLn "EOF"

processComputation :: Config -> String -> Int 
processComputation conf input = 2 + read input