module Console
  ( consoleRead,
    processRead,
  )
where

import Config (Config (..), defaultConf)
import Control.Monad (when)
import MathBlock (InterpolationMethod (..), Point, linearInt, newtonInt, lagrangeInt)
import Numeric (showFFloat)
import Parse (parsePoint, parseRead)
import System.IO (hFlush, stdout)

processRead :: [String] -> IO ()
processRead [] = do
  putStrLn "lol where? Ok using defaults"
  points <- collectPoints defaultConf
  startInteractive defaultConf points
processRead args = do
  let config = parseRead args defaultConf
  points <- collectPoints config
  startInteractive config points

collectPoints :: Config -> IO [Point]
collectPoints config = do
  let n = startPoints config
  putStrLn $ "Please enter " <> show n <> " points, format example x;y / x,y / x y"
  collectNPoints n []
  where
    collectNPoints :: Int -> [Point] -> IO [Point]
    collectNPoints 0 points = return $ reverse points
    collectNPoints n points = do
      putStr $ "Point " <> show (startPoints config - n + 1) <> "> "
      hFlush stdout
      input <- getLine
      case parsePoint input of
        Just point -> collectNPoints (n - 1) (point : points)
        Nothing -> do
          putStrLn "I cannot parse this, type as in example x;y / x, y / x y"
          collectNPoints n points

startInteractive :: Config -> [Point] -> IO ()
startInteractive conf points = do
  putStrLn $ "Current config is " <> show conf
  putStrLn $ "Your points: " <> show points
  putStrLn "Type EOF if you want to exit"
  when (length points >= startPoints conf) $ do
    let interpolated = interpolateStream conf points
    mapM_ (putStrLn . formatPoint conf) interpolated
  consoleRead conf points

consoleRead :: Config -> [Point] -> IO ()
consoleRead conf points = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case input of
    "EOF"-> finishComputation
    _ -> case parsePoint input of
      Just new -> do
        let newPoints = new : points
        when (length newPoints >= startPoints conf) $ do
          let interpolated = interpolateStream conf newPoints
          mapM_ (putStrLn . formatPoint conf) interpolated
        consoleRead conf newPoints
      Nothing -> consoleRead conf points

finishComputation :: IO ()
finishComputation = putStrLn "EOF"

interpolateStream :: Config -> [Point] -> [Point]
interpolateStream _ [] = []
interpolateStream conf points@(newPoint : prevPoints) =
  case prevPoints of
    [] -> [newPoint]
    (prevPoint : _) ->
      let stepVal = step conf
          startX = fst prevPoint
          endX = fst newPoint
          interpolatedXs
            | startX < endX = takeWhile (<= endX) [startX, startX + stepVal ..]
            | otherwise = takeWhile (<= startX) [endX, endX + stepVal ..]
       in [(x, processComputation conf points x) | x <- interpolatedXs]

formatPoint :: Config -> Point -> String
formatPoint conf (x, y) = show (method conf) <> ": (" <> showFFloat (Just 2) x "" <> "; " <> showFFloat (Just 2) y "" <> ")"

processComputation :: Config -> [Point] -> Double -> Double
processComputation conf points x =
  case method conf of
    Linear -> linearInt points x
    Newton -> newtonInt points x
    Lagrange -> lagrangeInt points x
