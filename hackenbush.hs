module Main(main) where

import Control.Exception (catch, throwIO,SomeException)
import Control.Monad (zipWithM_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Maybe (isJust, fromJust)
import Graphics.UI.GLUT 
import Prelude hiding (catch)
import System.Exit (ExitCode(ExitSuccess),exitWith)
import System.Random
import qualified Foreign.C.Types
import BTree

type NDouble = Foreign.C.Types.CDouble
newtype Scene = Scene (IO Scene)

data GameState = GameState { sIndex :: Int,  turn :: Int, tlist :: [BinTree], vIndex :: Int, bIndex :: Int, tIndex::Int } deriving (Show, Eq)

main :: IO ()
main = do
  keystate <- newIORef []
  cp <- newIORef $ openingScene keystate

  initialWindowSize $= Size 1280 720
  initialDisplayMode $= [RGBMode, DoubleBuffered]
  getArgsAndInitialize
  _window <- createWindow "Hackenbush"

  displayCallback $= display cp
  keyboardMouseCallback $= Just (keyboard keystate)

  addTimerCallback 24 $ timerProc $ display cp

  initMatrix

  mainLoop
  destroyWindow _window
  `catch` (\e -> return (const () (e::SomeException)) )

initMatrix :: IO ()
initMatrix = do
  viewport $= (Position 0 0, Size 1280 720)
  matrixMode $= Projection
  loadIdentity
  perspective 30.0 (4/3) 600 1400
  lookAt (Vertex3 0 0 (927 :: NDouble)) (Vertex3 0 0 (0 :: NDouble)) (Vector3 0 1 (0 :: NDouble))

exitLoop :: IO a 
exitLoop = exitWith ExitSuccess

keyboard :: IORef [Key] -> Key -> KeyState -> t -> t1 -> IO ()
keyboard keystate key ks _ _ =
  case (key, ks) of 
  	(Char 'q', _) -> exitLoop
  	(_,Down) -> modifyIORef keystate $ nub . (++[key])
  	(_,Up) -> modifyIORef keystate $ filter (/=key)

modeScene :: IORef [Key] -> IO Scene
modeScene ks = do
  keystate <- readIORef ks
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity 
  g <- newStdGen
  color $ Color3 (1.0::NDouble) 1.0 1.0
  preservingMatrix $ do
    translate $ Vector3 (-180::NDouble) 100 0
    scale (0.4::NDouble) 0.4 0.4
    renderString Roman "Choose Mode"
  preservingMatrix $ do
    translate $ Vector3 (-180::NDouble) 0 0
    scale (0.2::NDouble) 0.2 0.2
    renderString Roman "z. Player vs Player (Press p)"
  preservingMatrix $ do
    translate $ Vector3 (-180::NDouble) (-100) 0
    scale (0.2::NDouble) 0.2 0.2
    renderString Roman "x. Player vs Computer (press a)"
  
  swapBuffers
  if (Char 'z' `elem` keystate) then do
    gens <- nStdGen 5
    gs <- newIORef (initialState (nRand 2 9 g) gens)
    return $ Scene $ mainScene gs ks
    else if (Char 'x' `elem` keystate) then do
      gens <- nStdGen 5
      gs <- newIORef (initialState (nRand 2 9 g) gens)
      return $ Scene $ aiScene gs ks
      else return $ Scene $ modeScene ks 

openingScene :: IORef [Key] -> IO Scene
openingScene ks = do
  keystate <- readIORef ks
  g <- newStdGen
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  color $ Color3 (1.0 :: NDouble) 1.0 1.0
  preservingMatrix $ do
    translate $ Vector3 (-250 :: NDouble) 0 0
    scale (0.8 :: NDouble) 0.8 0.8
    renderString Roman "Hackenbush"
  preservingMatrix $ do
    translate $ Vector3 (-180 :: NDouble) (-100) 0
    scale (0.4 :: NDouble) 0.4 0.4
    renderString Roman "Press S key"

  swapBuffers
  
  if (Char 's' `elem` keystate) || (Char 'S' `elem` keystate) then do
  	--gens <- nStdGen 5
  	--gs <- newIORef (initialState (nRand 2 9 g) gens)
  	--return $ Scene $ mainScene gs ks
    return $ Scene $ modeScene ks
  else return $ Scene $ openingScene ks

nRand :: Int -> Int -> StdGen -> [Int]
nRand st en g = take 5 (randomRs (st::Int, en::Int) g)

nStdGen :: Int -> IO [StdGen]
nStdGen 0 = return []
nStdGen n = do
	g <- newStdGen
	g1 <- nStdGen (n-1)
	return (g : g1)

mainScene :: IORef GameState -> IORef [Key] -> IO Scene
mainScene gs ks = do
  keystate <- readIORef ks
  gamestate <- readIORef gs
  newgs <- (updateGameState gamestate keystate)
  t <- newIORef newgs
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  if (hasMove newgs) && (not (winState newgs)) then
    renderGame newgs
  else renderWinStateAI newgs
  swapBuffers
  return $ Scene $ mainScene t ks
  
  --if (hasMove gamestate) && (not (winState gamestate)) then
  --  return $ Scene $ mainScene t ks
  --  else return $ Scene $ winScene t


aiScene :: IORef GameState -> IORef [Key] -> IO Scene
aiScene gs ks = do
  keystate <- readIORef ks
  gamestate <- readIORef gs
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  if (getTurn gamestate) == 0 then do
    t <- (updateGameState gamestate keystate)
    newgs <- newIORef t
    if hasMove t then do
      renderGame t
      swapBuffers
      return $ Scene $ aiScene newgs ks
      else do
        renderWinStateAI t
        swapBuffers
        return $ Scene $ aiScene newgs ks
    else do
      t <- (makeAIMove gamestate)
      newgs <- newIORef t
      if hasMove t then do
        renderGame t
        swapBuffers
        return $ Scene $ aiScene newgs ks
        else do 
          renderWinStateAI t
          swapBuffers
          return $ Scene $ aiScene newgs ks

getTurn :: GameState -> Int
getTurn gs@GameState{turn=t} = t

renderGame :: GameState -> IO ()
renderGame gs= do
  renderGameMeta gs
  renderGameState gs

winScene :: IORef GameState -> IO Scene
winScene gs = do
  gamestate <- readIORef gs
  clear [ColorBuffer, DepthBuffer]
  matrixMode $= Modelview 0
  loadIdentity
  renderWinState gamestate
  swapBuffers
  return $ Scene $ winScene gs

aiMove :: [BinTree] -> [BinTree]
aiMove [] = []
aiMove (x:xs) = if hasBranchColored x (fst (playerColor 1)) then
                  removeFirst x (fst (playerColor 1)) : xs
                  else if hasBranchColored x (snd (playerColor 1)) then
                    removeFirst x (snd (playerColor 1)) : xs
                    else x : aiMove xs

makeAIMove :: GameState -> IO GameState
makeAIMove gs@GameState{tlist=tl} = do
  if hasMove gs then return GameState{turn=0, vIndex=0, bIndex=0,tIndex=0, sIndex=0, tlist=aiMove tl} else return gs

renderWinState :: GameState -> IO ()
renderWinState gs@GameState{turn=t} = do
  if t == 0 then
    preservingMatrix $ do
    scale (0.2::NDouble) 0.2 0.2
    renderString Roman "Player2 Won"
    else 
      preservingMatrix $ do
      scale (0.2::NDouble) 0.2 0.2
      renderString Roman "Player1 Won"

renderWinStateAI :: GameState -> IO ()
renderWinStateAI gs@GameState{turn=t} = do
  if t == 0 then
    preservingMatrix $ do
    scale (0.2::NDouble) 0.2 0.2
    renderString Roman "Computer Won"
    else 
      preservingMatrix $ do
      scale (0.2::NDouble) 0.2 0.2
      renderString Roman "Player Won"

winState :: GameState -> Bool
winState gs@GameState{tlist=tl} = emptyTrees tl

emptyTrees :: [BinTree] -> Bool
emptyTrees [] = True
emptyTrees (x:xs) = emptyTree x && emptyTrees xs

renderGameMeta :: GameState -> IO ()
renderGameMeta g@GameState{turn=t, sIndex=s} = do
  if(t==1)then
    preservingMatrix $ do
    translate $ Vector3 (-180 :: NDouble) 200 0
    scale (0.1 :: NDouble) 0.1 0.1
    renderString Roman "Player2"
    else
      preservingMatrix $ do
      translate $ Vector3 (-180 :: NDouble) 200 0
      scale (0.1 :: NDouble) 0.1 0.1
      renderString Roman "Player1"
  if(s==0)then
    preservingMatrix $ do
    translate $ Vector3 (-180 :: NDouble) 170 0
    scale (0.1 :: NDouble) 0.1 0.1
    renderString Roman "Choose a tree"
    else if(s==2)then
      preservingMatrix $ do
      translate $ Vector3 (-180 :: NDouble) 170 0
      scale (0.1 :: NDouble) 0.1 0.1
      renderString Roman "Choose vertex"
    else if(s==4) then
      preservingMatrix $ do
      translate $ Vector3 (-180 :: NDouble) 170 0
      scale (0.1 :: NDouble) 0.1 0.1
      renderString Roman "Choose a branch"
    else
      preservingMatrix $ do
      translate $ Vector3 (-180 :: NDouble) 170 0
      scale (0.1 :: NDouble) 0.1 0.1
      renderString Roman "Press c"

display :: IORef (IO Scene) -> DisplayCallback
display cp = do
  m <- readIORef cp
  Scene next <- m
  writeIORef cp next

timerProc :: IO a -> TimerCallback
timerProc m = m >> addTimerCallback 16 (timerProc m)

renderGameState :: GameState -> IO ()
renderGameState GameState{sIndex=i, turn=t, tlist=tl} = do
  renderTrees tl 0
  return ()

renderTrees :: [BinTree] -> Int -> IO ()
renderTrees [] _ = return ()
renderTrees bt@(x:xs) i = do
  renderTree i x 
  renderTrees xs (i+1)  

renderTree :: Int -> BinTree -> IO ()
renderTree i bt = do
  preservingMatrix $ do
  renderPrimitive Lines $ do
  color $ Color3 (1.0::NDouble) 0 0
  mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) (getColorPoints bt BTree.Red (getRootXY  i) 30 )
  color $ Color3 0 0 (1.0::NDouble)
  mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) (getColorPoints bt BTree.Blue (getRootXY i) 30 )
  color $ Color3 0 (1.0::NDouble) 0
  mapM_ (\(x,y) -> vertex $ Vertex3 x y 0) (getColorPoints bt BTree.Green (getRootXY i) 30 )

getRootXY :: Int -> (Int, Int)
getRootXY i = 
  case (i `mod` 2) of
    0 -> (((-200) + (i * 100) ), 0)
    1 -> (((-200) + (i * 100) ), -100)

toNDouble :: (Int, Int) -> (NDouble, NDouble)
toNDouble (x,y) = (fromIntegral x, fromIntegral y)

getColorPoints :: BinTree -> ColorTag -> (Int, Int) -> Int -> [(NDouble, NDouble)]
getColorPoints NoTree _ _ _ = []
getColorPoints (Node NoTree _ _ NoTree) _ _ _ = []
getColorPoints (Node NoTree _ rb rt) ct (x,y) xd
  | rb == (RightBranch ct) = [toNDouble (x,y), toNDouble (x+xd, y+20)] ++ getColorPoints rt ct (x+xd, y+20) (xd-10)
  | otherwise = getColorPoints rt ct (x+xd, y+20) (xd-10)
getColorPoints (Node lt lb _ NoTree) ct (x,y) xd
  | lb == (LeftBranch ct) = [toNDouble (x,y), toNDouble (x-xd, y+20)] ++ getColorPoints lt ct (x-xd, y+20) (xd-10)
  | otherwise = getColorPoints lt ct (x-xd, y+20) (xd-10)
getColorPoints (Node lt lb rb rt) ct (x,y) xd
  | lb == (LeftBranch ct) && rb == (RightBranch ct) = [toNDouble (x,y), toNDouble (x-xd, y+20), toNDouble (x,y), toNDouble (x+xd,y+20)] ++ getColorPoints lt ct (x-xd,y+20) (xd-10) ++ getColorPoints rt ct (x+xd, y+20) (xd-10)
  | rb == (RightBranch ct) = getColorPoints lt ct (x-xd,y+20) (xd-10) ++ [toNDouble (x,y), toNDouble (x+xd,y+20)] ++ getColorPoints rt ct (x+xd, y+20) (xd-10)
  | lb == (LeftBranch ct) = [toNDouble (x,y), toNDouble (x-xd,y+20)] ++ getColorPoints lt ct (x-xd,y+20) (xd-10) ++ getColorPoints rt ct (x+xd, y+20) (xd-10)
  | otherwise = getColorPoints lt ct (x-xd,y+20) (xd-10) ++ getColorPoints rt ct (x+xd, y+20) (xd-10)

initialState v g = GameState { sIndex=0, turn=0, tlist = generateRandomTrees v g, vIndex=0, bIndex=0, tIndex=0}

hasMove :: GameState -> Bool
hasMove GameState{turn=t, tlist=tl} = (hasTreeWithBranchColor tl (fst (playerColor t)) ) || (hasTreeWithBranchColor tl (snd (playerColor t)) )

hasTreeWithBranchColor :: [BinTree] -> ColorTag -> Bool
hasTreeWithBranchColor [] _ = False
hasTreeWithBranchColor (x:xs) ct = (hasBranchColored x ct) || hasTreeWithBranchColor xs ct

isValidTreeIndex :: Int -> Bool
isValidTreeIndex b = (b >=0 && b < 5)

isValidBranchIndex :: Int -> Bool
isValidBranchIndex b = (b == 0 || b == 1)

modify :: Int -> [BinTree] -> BinTree -> [BinTree]
modify 0 (x:xs) bt = bt : xs
modify i (x:xs) bt = x : modify (i-1) xs bt 

playerColor :: Int -> (ColorTag, ColorTag)
playerColor b = if b == 0 then (BTree.Red, BTree.Green) else (BTree.Blue, BTree.Green)

move :: Int -> Int -> Int -> Int -> [BinTree] -> [BinTree]
move pi ti vi bi bts@(x:xs) = if isValidBranchIndex bi && isValidTreeIndex ti then modify ti bts $ getOne (bts!!ti) ( removeSubtree vi bi (fst (playerColor pi)) (bts!!ti) ) ( removeSubtree vi bi (snd (playerColor pi)) (bts!!ti) ) else bts

hasnumkey ::[Key]->Int
hasnumkey ks=if(Char '1' `elem` ks)then 1 
  else if(Char '2' `elem` ks)then 2 
    else if(Char '3' `elem` ks)then 3 
      else if(Char '4' `elem` ks)then 4 
        else if(Char '5' `elem` ks)then 5 
          else if(Char '6' `elem` ks)then 6 
            else if(Char '7' `elem` ks)then 7 
              else if(Char '8' `elem` ks)then 8
                else if(Char '9' `elem` ks)then 9
                  else if(Char '0' `elem` ks) then 0
                    else -1  

hasc :: [Key] -> Bool
hasc ks = (Char 'c' `elem` ks)

selectTree :: GameState -> [Key] -> GameState
selectTree gs@GameState{turn=t, sIndex=s, tlist=tl, vIndex=v, tIndex=ti, bIndex=b} ks = if isValidTreeIndex $ hasnumkey ks then GameState{turn=t, sIndex=(s+1),vIndex=v,tlist=tl,tIndex=(hasnumkey ks),bIndex=b} else gs

selectVertex :: GameState -> [Key] -> GameState
selectVertex gs@GameState{turn=t, sIndex=s, tlist=tl, vIndex=v, tIndex=ti, bIndex=b} ks = if (hasnumkey ks) /= -1 then GameState{turn=t, sIndex=(s+1),vIndex=(hasnumkey ks),tlist=tl,tIndex=ti,bIndex=b} else gs

selectBranch :: GameState -> [Key] -> GameState
selectBranch gs@GameState{turn=t, sIndex=s, tlist=tl, vIndex=v, tIndex=ti, bIndex=b} ks = if isValidBranchIndex $ hasnumkey ks then GameState{turn=t, sIndex=(s+1),vIndex=v,tlist=tl,tIndex=ti,bIndex=(hasnumkey ks)} else gs

sameTrees :: GameState -> GameState -> Bool
sameTrees g1@GameState{turn=t, sIndex=s, tlist=tl, vIndex=v, tIndex=ti, bIndex=b} g2@GameState{turn=t1, sIndex=s1, tlist=tl1, vIndex=v1, tIndex=ti1, bIndex=b1} = (tl == tl1)

setTurn :: GameState -> GameState -> GameState
setTurn g1@GameState{turn=t, sIndex=s, tlist=tl, vIndex=v, tIndex=ti, bIndex=b} g2@GameState{turn=t1, sIndex=s1, tlist=tl1, vIndex=v1, tIndex=ti1, bIndex=b1} = if sameTrees g1 g2 then g2 else GameState{turn = ( (t+1) `mod` 2), tlist=tl1, vIndex=0, tIndex=0, bIndex=0, sIndex=s1}

continue :: GameState -> [Key] -> GameState
continue gs@GameState{turn=t, sIndex=s, tlist=tl, vIndex=v, tIndex=ti, bIndex=b} ks = if hasc ks then GameState{turn=t, sIndex = ((s+1) `mod` 6), vIndex=v, tIndex=ti, bIndex=b, tlist=tl } else gs

endMove :: GameState -> [Key] -> GameState
endMove gs@GameState{turn=t, sIndex=s, tlist=tl, vIndex=v, tIndex=ti, bIndex=b} ks = if hasc ks then GameState{turn=t, sIndex = ((s+1) `mod` 6), vIndex=v, tIndex=ti, bIndex=b, tlist=(move t ti v b tl) } else gs


updateGameState :: GameState -> [Key] -> IO GameState
updateGameState g@GameState{turn=t, sIndex=s, tlist=tl, vIndex=v, tIndex=ti, bIndex=b} ks = do
  if s == 0 then
  	return $ selectTree g ks
  	else if s == 1 then
  	  return $ continue g ks
  	  else if s == 2 then
  	    return $ selectVertex g ks
  	    else if s == 3 then
  	      return $ continue g ks
  	      else if s == 4 then
  	        return $ selectBranch g ks
  	        else return $ setTurn g $ endMove g ks