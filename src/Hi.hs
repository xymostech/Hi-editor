import UI.HSCurses.Curses
import Control.Concurrent
import Foreign.C.Types
import Data.Char
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Prelude hiding (lines)

cursesSetup :: IO ()
cursesSetup = do
  initScr
  cBreak True
  raw True
  echo False
  refresh

cursesCleanup :: IO ()
cursesCleanup = do
  endWin

isPrintableCharacter :: Int -> Bool
isPrintableCharacter char =
  (char >= space && char <= tilde)
  where
    space = ord ' '
    tilde = ord '~'

data Mode = Normal | Insert
  deriving Show

data AppState = AppState
  { stateLines :: [[Char]]
  , statePosition :: (Int, Int)
  , stateMode :: Mode
  }
  deriving Show

makeState :: AppState
makeState = AppState
  { stateLines = [""]
  , statePosition = (0, 0)
  , stateMode = Normal
  }

modifyPosition :: Int -> (a -> a) -> [a] -> [a]
modifyPosition _ _ [] = []
modifyPosition 0 f (x:xs) = (f x) : xs
modifyPosition pos f (x:xs) = x : (modifyPosition (pos - 1) f xs)

insertAtPosition :: Int -> a -> [a] -> [a]
insertAtPosition 0 elem list = elem : list
insertAtPosition _ _ [] = []
insertAtPosition pos elem (x:xs) = x : (insertAtPosition (pos - 1) elem xs)

addChar :: Int -> AppState -> AppState
addChar char state@(AppState lines (r,c) _) =
  state{stateLines = lines', statePosition = (r, c + 1)}
  where
    lines' = modifyPosition r (insertAtPosition c (chr char)) lines

addLine :: AppState -> AppState
addLine state@AppState{stateLines = lines, statePosition = (r,c)} =
  state{stateLines = lines', statePosition = (r + 1, 0)}
  where
    lines' = insertAtPosition (r + 1) "" lines

handleMovement :: Int -> AppState -> AppState
handleMovement char state@AppState{stateLines = lines, statePosition = (r, c)}
  | char == ord 'h' = state{statePosition = (r, max (c-1) 0)}
  | char == ord 'l' = state{statePosition = (r, min (c+1) (length (lines !! r)))}
  | char == ord 'k' = state{statePosition = (max (r-1) 0, c)}
  | char == ord 'j' = state{statePosition = (min (r+1) ((length lines) - 1), c)}
  | otherwise = state

isMovementCharacter :: Int -> Bool
isMovementCharacter char =
  char >= h && char <= l
  where
    h = ord 'h'
    l = ord 'l'

handleChar :: CInt -> AppState -> AppState
handleChar cchar state@(AppState{stateMode = Insert})
  | isPrintableCharacter char = addChar char state
  | char == 10 = addLine state
  | char == 27 = state{stateMode = Normal}
  | otherwise = state
  where
    char = fromIntegral cchar
handleChar cchar state@(AppState{stateMode = Normal})
  | char == ord 'i' = state{stateMode = Insert}
  | otherwise = handleMovement char state
  where
    char = fromIntegral cchar

drawLine :: [Char] -> IO ()
drawLine line = wAddStr stdScr line >> addLn

draw :: StateT AppState IO ()
draw = do
  liftIO $ erase
  l <- gets stateLines
  (r,c) <- gets statePosition
  liftIO $ mapM_ drawLine l
  liftIO $ wMove stdScr r c
  liftIO $ update

loop :: StateT AppState IO ()
loop = do
  draw
  key <- liftIO getch
  modify $ handleChar key
  when (key /= 3) loop

main :: IO ()
main = do
  cursesSetup
  runStateT loop makeState
  cursesCleanup
