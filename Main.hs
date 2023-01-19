module Main where

import Smeeborg
import Data.List
import System.IO
import Data.List.Split
import Data.Maybe
import Control.Monad

load :: String -> IO Board
load filename = do
    contents <- readFile filename
    return (map (map (\x -> read [x] :: Int)) (splitOn "\n" contents))

printBoard :: Board -> Coord -> String
printBoard board (x,y) = intercalate "\n" (updateBoard (x,y) '@' (boardToString board))

updateBoard :: Coord -> Char -> [String] -> [String]
updateBoard (x,y) c ss = take x ss ++ [take y (ss!!x) ++ [c] ++ drop (y+1) (ss!!x)] ++ drop (x+1) ss

boardToString :: Board -> [String]
boardToString board = map (map printCell) board
  where
    printCell :: Int -> Char
    printCell 0 = '-'
    printCell 1 = '*'
    printCell 2 = 'p'
    printCell 3 = 'o'
    printCell 4 = 'y'
    printCell 5 = 'b'
    printCell 6 = '-'
    printCell 7 = 't'

menu :: IO ()
menu = putStrLn " 1) Load file: Loads a map. \n 2) Check: Checks if the currently loaded map is solvable. \n 3) Solve: Solves the map and presents the solution as a set of instructions.\n 4) Quit: Quits the game.\n 5) Play: Allows the player to propose a solution as a set of instructions.\n Select 1 ~ 5:"

loop :: Maybe Board -> IO (Maybe Board)
loop board = do
  putStr "> "
  c <- getChar
  case c of 
    '1' -> do b <- loadFile 
              menu
              loop (Just b)
    '2' -> checkBoard (fromJust board) >> menu >> loop board
    '3' -> solveBoard (fromJust board) >> menu >> loop board
    '4' -> putStrLn "\nByebye~" >> return board
    '5' -> playGame [] (fromJust board) >> menu >> loop board
    _ -> putStrLn "\nInput wrong, select the options again!" >> menu >> loop board


loadFile :: IO Board
loadFile = do
  putStr "\nPlease input the name of map file: "
  filename <- getLine
  board <- load filename
  let start = getStart board
  putStrLn $ printBoard board start
  return board

checkBoard :: Board -> IO ()
checkBoard board = do
  let solution = solver board
  case solution of 
    Nothing -> putStrLn "\nThe currently loaded map is not solvable!"
    _ -> putStrLn "\nThe currently loaded map is solvable!"

solveBoard :: Board -> IO ()
solveBoard board = do
  let solution = solver board
  case solution of
    Just actions -> do putStrLn $ "\nSolution: " ++ show (reduceActions actions)
                       printActions 1 actions board


playGame :: [Action] -> Board -> IO ()
playGame pre_actions board = 
  let coords = getPath pre_actions board
      current_coord = last coords 
  in 
  do
    putStr "\n> play "
    x <- getLine
    case x of 
        "" -> do putStr "First direction: "
                 y <- getLine
                 case y of 
                     "" -> return ()
                     _ -> do xs <- playGameLoop 
                             let actions = flattenActions (pre_actions ++ readDirctions (y:xs))
                             case steps actions (getStart board) board [getStart board] of 
                                 Nothing -> do putStrLn "Sorry, error: cannot move.\nYour current board:"
                                               putStrLn $ printBoard board current_coord
                                               playGame pre_actions board 
                                 _ -> putStrLn "test:" >> printActions (length pre_actions + 1) actions board >> playGame actions board
        _  -> let fun = readDirctions (splitOn " " x)
              in do putStr "First direction: "
                    y <- getLine
                    case y of 
                        "" -> return ()
                        _ -> do xs <- playGameLoop 
                                let actions = flattenActions (pre_actions ++ readDirctionFuns fun (y:xs))
                                case steps actions (getStart board) board [getStart board] of 
                                    Nothing -> do putStrLn "Sorry, error: cannot move.\nYour current board:"
                                                  putStrLn $ printBoard board current_coord
                                                  playGame pre_actions board 
                                    _ -> do putStrLn "test:" 
                                            printActions (length pre_actions + 1) actions board
                                            let path = getPath actions board
                                            case path of
                                                [] -> playGame actions board
                                                _ -> if last path == getTarget board 
                                                     then putStrLn "Congratulations! You win the game!" 
                                                     else playGame actions board


readDirctionFuns :: [Action] -> [String] -> [Action]
readDirctionFuns fun ss = map readDirction ss
  where
    readDirction :: String -> Action
    readDirction "Right" = Base DRight
    readDirction "Up" = Base DUp
    readDirction "Left" = Base DLeft
    readDirction "Down" = Base DDown
    readDirction "Function" = Function (fun!!0) (fun!!1) (fun!!2)
    readDirction x = case splitOn " " x of 
      ["Cond", c, d] -> case c of
        "Pink" -> let Base dd = readDirction d in Cond Pink dd
        "Orange" -> let Base dd = readDirction d in Cond Orange dd
        "Yellow" -> let Base dd = readDirction d in Cond Yellow dd
      ["Loop", n, a, b] -> Loop (read n) (readDirction a) (readDirction b)

readDirctions :: [String] -> [Action]
readDirctions = map readDirction
  where
    readDirction :: String -> Action
    readDirction "Right" = Base DRight
    readDirction "Up" = Base DUp
    readDirction "Left" = Base DLeft
    readDirction "Down" = Base DDown
    readDirction x = case splitOn " " x of 
      ["Cond", c, d] -> case c of
        "Pink" -> let Base dd = readDirction d in Cond Pink dd
        "Orange" -> let Base dd = readDirction d in Cond Orange dd
        "Yellow" -> let Base dd = readDirction d in Cond Yellow dd
      ["Loop", n, a, b] -> Loop (read n) (readDirction a) (readDirction b)

playGameLoop :: IO [String]
playGameLoop = do
    putStr "Next  direction: "
    x <- getLine
    case x of 
        "" -> return []
        _  ->playGameLoop >>= \xs -> return (x:xs)
    

printActions :: Int -> [Action] -> Board -> IO ()
printActions n actions board = mapM_ (\x -> putStrLn (printBoard board x) >> putStrLn "") path
  where
    path = drop n $ getPath actions board

main :: IO ()
main = do
    putStrLn "Welcome to play the Smeeborg game:\n"
    menu
    loop Nothing
    return ()

