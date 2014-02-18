module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char
import Foreign.C.Types
import Prelude hiding (lines)
import qualified Data.Map as Map
import UI.HSCurses.Curses

import AppState
import Base

addDefaultBindings :: AppState -> AppState
addDefaultBindings state =
  foldl (\s (mode, char, func) -> (modeFunc mode) char func s) state $
    [ (Normal, ord 'i', insertMode)
    , (Normal, ord 'a', moveRight . insertMode)
    , (Normal, ord 'h', moveLeft)
    , (Normal, ord 'l', moveRight)
    , (Normal, ord 'k', moveUp)
    , (Normal, ord 'j', moveDown)
    , (Insert, 127, removeChar)
    , (Insert, 10, addLine)
    , (Insert, 27, moveLeft . normalMode)
    ] ++
    [(Insert, c, addChar c) | c <- [(ord ' ')..(ord '~')]]
  where
    modeFunc Insert = addInsertMapping
    modeFunc Normal = addNormalMapping

handleChar :: CInt -> AppState -> IO AppState
handleChar cchar state@(AppState{stateMode = mode}) =
  case currMapping of
    Just func -> func state
    Nothing -> return state
  where
    char = fromIntegral cchar

    mapping Normal = normalMapping state
    mapping Insert = insertMapping state

    currMapping = Map.lookup char $ mapping mode

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
  currState <- get
  newState <- liftIO $ handleChar key currState
  put newState
  when (key /= 3) loop

main :: IO ()
main = do
  cursesSetup
  runStateT loop $ addDefaultBindings makeState
  cursesCleanup
