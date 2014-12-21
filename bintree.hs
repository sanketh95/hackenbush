module BTree where

import System.Random

data ColorTag = Blue | Red | Green
  deriving (Show, Eq)

data Branch = NoBranch | LeftBranch ColorTag | RightBranch ColorTag
  deriving (Show, Eq)

data BinTree = NoTree
               | Node BinTree Branch Branch BinTree
  deriving (Show, Eq)

randomNodeCount st en g = randomRs (st::Int, en::Int) g
randomColorIndices n g = take n (randomRs (0, 2) g)

colorTagFromIndex :: Int -> ColorTag
colorTagFromIndex i = case i of 0 -> Blue
                                1 -> Red 
                                2 -> Green

{-
    joinBinTrees :: BinTree
-}
joinBinTrees :: BinTree -> Branch -> Branch -> BinTree -> BinTree
joinBinTrees lt lb rb rt = (Node lt lb rb rt)

removeSubtree :: Int -> Int -> ColorTag -> BinTree -> BinTree
removeSubtree _ _ _ NoTree = NoTree
removeSubtree 0 0 Blue bt@(Node lt lb (RightBranch Blue) rt) = Node lt lb NoBranch NoTree
removeSubtree 0 0 Red bt@(Node lt lb (RightBranch Red) rt) = Node lt lb NoBranch NoTree
removeSubtree 0 0 Green bt@(Node lt lb (RightBranch Green) rt) = Node lt lb NoBranch NoTree
removeSubtree 0 1 Blue bt@(Node lt (LeftBranch Blue) rb rt) = Node NoTree NoBranch rb rt
removeSubtree 0 1 Red bt@(Node lt (LeftBranch Red) rb rt) = Node NoTree NoBranch rb rt
removeSubtree 0 1 Green bt@(Node lt (LeftBranch Green) rb rt) = Node NoTree NoBranch rb rt
removeSubtree 0 _ _ bt = bt
removeSubtree vi b pc bt@(Node lt lb rb rt)
  | vi > 0 = joinBinTrees (removeSubtree (vi-2) b pc lt) lb rb (removeSubtree (vi-1) b pc rt) 
  | otherwise = bt

printTree :: BinTree -> IO ()
printTree bt = putStrLn $ show bt

getMid :: [ColorTag] -> (ColorTag, ColorTag)
getMid [] = (Blue, Red)
getMid [x] = (x, x)
getMid x = (x!!(p-1), x!!p) where p =((length x) `div` 2)

getLeft :: [ColorTag] -> [ColorTag]
getLeft s = splitList s 0 (((length s) `div` 2) - 2)

getRight :: [ColorTag] -> [ColorTag]
getRight s = splitList s (((length s) `div` 2) + 1) ((length s) - 1)

splitList :: [ColorTag] -> Int -> Int -> [ColorTag]
splitList colors s e
  | (s < 0) || (e >= (length colors)) = []
  | (s > e) = []
  | otherwise = (colors !! s) : (splitList colors (s+1) e)

createBinTree :: Int -> [ColorTag] -> BinTree
createBinTree 0 _ = NoTree
createBinTree 1 _ = Node NoTree NoBranch NoBranch NoTree
createBinTree n s@(x:xs) = joinBinTrees (createBinTree p (getLeft s)) (LeftBranch (fst (getMid s))) (RightBranch (snd (getMid s))) (createBinTree (n-p-1) (getRight s)) where p = ((n-1) `div` 2)

createRandomBinTree :: Int -> StdGen -> BinTree
createRandomBinTree n g = createBinTree n (map colorTagFromIndex (randomColorIndices n g))

generateRandomTrees :: [Int] -> [StdGen] -> [BinTree]
generateRandomTrees [] [] = []
generateRandomTrees is@(x:xs) gs@(g:gx)
  | ((length is) /= (length gs)) = []
  | otherwise = (createRandomBinTree x g) : (generateRandomTrees xs gx)

getOne :: BinTree -> BinTree -> BinTree -> BinTree
getOne t b1 b2 = if t == b2 then b1 else b2

hasBranchColored :: BinTree -> ColorTag -> Bool
hasBranchColored NoTree _ = False
hasBranchColored bt@(Node lt lb rb rt) ct
  | emptyTree bt = False
  | lb == NoBranch && rb == NoBranch = False
  | lb == (LeftBranch ct) && lt /= NoTree = True
  | rb == (RightBranch ct) && rt /= NoTree = True
  | otherwise = ((hasBranchColored lt ct) || (hasBranchColored rt ct))


emptyTree :: BinTree -> Bool
emptyTree NoTree = True
emptyTree (Node NoTree _ _ NoTree) = True
emptyTree _ = False

removeFirst :: BinTree -> ColorTag -> BinTree
removeFirst NoTree _ = NoTree
removeFirst bt@(Node NoTree _ _ NoTree) _ = bt
removeFirst bt@(Node lt lb rb rt) ct
  | lb == NoBranch && rb == NoBranch = bt
  | rt /= NoTree && rb == (RightBranch ct) = (Node lt lb NoBranch NoTree)
  | lt /= NoTree && lb == (LeftBranch ct) = (Node NoTree NoBranch rb rt)
  | otherwise = if hasBranchColored lt ct then removeFirst lt ct else removeFirst rt ct