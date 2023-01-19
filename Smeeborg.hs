module Smeeborg where

import Data.List
import System.IO
import Data.List.Split
import Data.Maybe
import Data.Function

import Debug.Trace


type Board = [[Int]]
data Color = Pink | Orange | Yellow deriving (Eq, Show, Ord)
data Dirction = DUp | DDown | DLeft | DRight  deriving (Eq, Show, Ord)
data Action = Base Dirction | Cond Color Dirction | Loop Int Action Action | Function Action Action Action deriving (Eq, Show, Ord)
type Coord = (Int, Int)


step :: Dirction -> Coord -> Board -> [Coord]
step dirction (x, y) board = case dirction of
    DUp    -> map (\z -> (x - z, y)) $ throughCoord up_col    0 []
    DDown  -> map (\z -> (x + z, y)) $ throughCoord down_col  0 []
    DLeft  -> map (\z -> (x, y - z)) $ throughCoord left_row  0 []
    DRight -> map (\z -> (x, y + z)) $ throughCoord right_row 0 []
  where
    t_board   = transpose board
    row       = board !! x
    col       = t_board !! y
    up_col    = reverse (take (x + 1) col)
    down_col  = drop x col
    right_row = drop y row
    left_row  = reverse (take (y + 1) row)


steps :: [Action] -> Coord -> Board -> [Coord] -> Maybe [Coord]
steps actions coord board acc = case actions of 
    []                  -> Just acc
    Base d1:Cond c d2:t -> do nc <- twoStepsWithColor d1 c coord board
                              steps (Cond c d2:t) nc board (acc ++ [nc])
    Cond _ d1:Cond c d2:t -> do nc <- twoStepsWithColor d1 c coord board
                                steps (Cond c d2:t) nc board (acc ++ [nc])
    Base d:t            -> let cs = step d coord board in if null cs then Nothing else steps t (last cs) board (acc ++ [last cs])
    Cond c d:t          -> let cs = step d coord board in if null cs then Nothing else steps t (last cs) board (acc ++ [last cs])
    _                   -> Nothing


twoStepsWithColor :: Dirction -> Color -> Coord -> Board -> Maybe Coord
twoStepsWithColor dirction color coord board
    | null cs = Nothing
    | otherwise = let ccs = filter (isColor color board) cs in if null ccs then Just (last cs) else Just (head ccs)
  where
    cs = step dirction coord board


isColor :: Color -> Board -> Coord -> Bool
isColor color board (x, y) = case color of
    Pink   -> c == 2
    Orange -> c == 3
    Yellow -> c == 4
  where
    c = board !! x !! y


throughCoord :: [Int] -> Int -> [Int] -> [Int]
throughCoord s n a = case s of
    [] ->  delete 0 $ nub (a ++ [n - 1])
    x:xs | x == 1 -> delete 0 $ nub (a ++ [n - 1])
         | x == 2 || x == 3 || x == 4 -> throughCoord xs (n + 1) (a ++ [n])
         | otherwise -> throughCoord xs (n + 1) a


getPath :: [Action] -> Board -> [Coord]
getPath actions board = fromJust $ steps actions start board [start]
  where
    start = getStart board


solver :: Board -> Maybe [Action]
solver = aStar [([], 100)] 


aStar :: [([Action], Int)] -> Board -> Maybe [Action]
aStar open board = case open of 
    [] -> Nothing
    h:t | isWin (getPath (fst h) board) bonuses target -> Just (fst h)
        | length open > 2000 -> Nothing
        | otherwise -> aStar (insertOpen t (extensionActions (fst h) board)) board
  where
    target  = getTarget board
    bonuses = getBonuses board


extensionActions :: [Action] -> Board -> [([Action], Int)]
extensionActions actions board = allNextHeursitics
  where
    path = getPath actions board
    start = getStart board
    dirctions = if null actions then [DRight, DLeft, DUp, DDown] else [DRight, DLeft, DUp, DDown] \\ [getDirction $ last actions]
    bases = map Base dirctions
    colors = if length path < 2 then [] else getColorInEdge (last (init path), last path) board
    conds = concatMap (\c -> map (Cond c) dirctions) colors
    nexts = bases ++ conds
    allNextsActions = map (\x -> actions ++ [x]) $ filter (\x -> isJust (steps (actions ++ [x]) start board [start])) nexts
    heuristics = map (\as -> heuristic as board) allNextsActions
    allNextHeursitics = zip allNextsActions heuristics


insertOpen :: [([Action], Int)] -> [([Action], Int)] -> [([Action], Int)]
insertOpen open extensions = foldl insertH open extensions
  where
    insertH :: [([Action], Int)] -> ([Action], Int) -> [([Action], Int)]
    insertH [] x = [x]
    insertH ((x,y):t) (a,b)
        | b < y = (a,b) : (x,y) : t
        | otherwise = (x,y) : insertH t (a,b)


heuristic :: [Action] -> Board -> Int
heuristic actions board
    | length bonuses > howManyBonuses path bonuses = ((length bonuses - howManyBonuses path bonuses) * 42) * 20 + length fold_actions
    | otherwise = (if null path then 42 else distance (last path) target) * 20 + length fold_actions
  where
    bonuses = getBonuses board
    target = getTarget board
    path = getPath actions board
    fold_actions = reduceActions actions


distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (y1 - y2) + abs (x1 - x2)


pathCoord :: [Coord] -> [Coord]
pathCoord []  = []
pathCoord [x]  = [x]
pathCoord path = concat $ zipWith (\x y -> edgeCoord (x, y)) path (tail path)


edgeCoord :: (Coord, Coord) -> [Coord]
edgeCoord ((x1, y1), (x2, y2))
  | x1 == x2 = map (\y -> (x1, y)) [min y1 y2..max y1 y2]
  | y1 == y2 = map (\x -> (x, y1)) [min x1 x2..max x1 x2]


getColorInEdge :: (Coord, Coord) -> Board -> [Color]
getColorInEdge ((x1, y1), (x2, y2)) board
    | x1 == x2 = let row = board !! x1
                     yy1 = if y1 < y2 then y1 + 1 else y1 - 1
                     cs = take (abs (yy1 - y2) + 1) $ drop (min yy1 y2) row
                 in if 2 `elem` cs then [Pink] else [] ++ if 3 `elem` cs then [Orange] else [] ++ if 4 `elem` cs then [Yellow] else []
    | y1 == y2 = let col = (transpose board) !! y1
                     xx1 = if x1 < x2 then x1 + 1 else x2 - 1
                     cs = take (abs (xx1 - x2) + 1) $ drop (min xx1 x2) col
                 in if 2 `elem` cs then [Pink] else [] ++ if 3 `elem` cs then [Orange] else [] ++ if 4 `elem` cs then [Yellow] else []


isWin :: [Coord] -> [Coord] -> Coord -> Bool
isWin [] _ _ = False
isWin path bonuses target =
    target == last path &&
    howManyBonuses path bonuses == length bonuses


howManyBonuses :: [Coord] -> [Coord] -> Int
howManyBonuses path bonuses = 
    length $ filter (\bonus -> any (inEdge bonus) (zip path (tail path))) bonuses


inEdge :: Coord -> (Coord, Coord) -> Bool
inEdge (x, y) ((x1, y1), (x2, y2)) 
    | x == x1 && x == x2 && y1 <= y && y <= y2 = True
    | x == x1 && x == x2 && y2 <= y && y <= y1 = True
    | y == y1 && y == y2 && x1 <= x && x <= x2 = True
    | y == y1 && y == y2 && x2 <= x && x <= x1 = True
    | otherwise = False


getDirction :: Action -> Dirction
getDirction (Base x) = x
getDirction (Cond _ x) = x


findElem :: Int -> Board -> [Coord]
findElem n board = map (\x -> (x `div` r, x `mod` r)) is
  where
    r = length (board!!0)
    is = elemIndices n (concat board)


getStart :: Board -> Coord
getStart board = head (findElem 6 board)


getTarget :: Board -> Coord
getTarget board = head (findElem 7 board)


getBonuses :: Board -> [Coord]
getBonuses board = findElem 5 board


flattenActions :: [Action] -> [Action]
flattenActions = concatMap flattenAction
  where
    flattenAction :: Action -> [Action]
    flattenAction (Loop n x y) = concat (replicate n [x, y])
    flattenAction (Function x y z) = [x, y, z]
    flattenAction x = [x]


reduceActions :: [Action] -> [Action]
reduceActions actions = case function of
    Just (Function a1 a2 a3) -> loopFold (reduceActionsFun a1 a2 a3 actions)
    _ -> loopFold actions
  where
    function = getFunction actions 
    reduceActionsFun :: Action -> Action -> Action -> [Action] -> [Action]
    reduceActionsFun a1 a2 a3 (h1:h2:h3:t)
      | h1 == a1 && h2 == a2 && h3 == a3 = Function a1 a2 a3 : reduceActionsFun a1 a2 a3 t
      | otherwise = h1 : reduceActionsFun a1 a2 a3 (h2:h3:t)
    reduceActionsFun _ _ _ x = x

    loopFold :: [Action] -> [Action]
    loopFold as = map list2Loop (loopFoldList as)

    list2Loop :: [Action] -> Action
    list2Loop x
      | length x == 1 = head x
      | otherwise = Loop (length x `div` 2) (x!!0) (x!!1)

    loopFoldList :: [Action] -> [[Action]]
    loopFoldList actions = case actions of 
      [] -> []
      _ -> x : loopFoldList y
      where
        (x, y) = loopFoldH actions
        loopFoldH :: [Action] -> ([Action], [Action])
        loopFoldH l 
            | length ls < 2    = ([head l], tail l)
            | length hgls == 1 = ([head l], tail l)
            | haveFunction (head hgls) = ([head l], tail l)
            | otherwise        = (take (2 * length hgls) l, drop (2 * length hgls) l)
          where
            ls = chunksOf 2 l
            group_ls = group ls 
            hgls = head group_ls
            haveFunction :: [Action] -> Bool
            haveFunction [] = False
            haveFunction (Function _ _ _:t) = True
            haveFunction (_:t) = haveFunction t


getFunction :: [Action] -> Maybe Action
getFunction as
    | length as <= 3 = Nothing
    | null group_lst = Nothing
    | otherwise = let max_list = head (maximumBy (compare `on` length) group_lst)
                  in Just (Function (max_list!!0) (max_list!!1) (max_list!!2))
  where
    lst1 = chunksOf 3 as 
    lst2 = chunksOf 3 (tail as) 
    lst3 = chunksOf 3 (tail (tail as))
    group_lst1 = filter (\x -> length x > 1) (group (sort lst1))
    group_lst2 = filter (\x -> length x > 1) (group (sort lst2))
    group_lst3 = filter (\x -> length x > 1) (group (sort lst3))
    group_lst = group_lst3 ++ group_lst2 ++ group_lst1
