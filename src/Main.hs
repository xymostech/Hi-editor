module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char
import Foreign.C.Types
import Prelude
import System.Environment
import UI.HSCurses.Curses

import qualified Data.Map as Map

import AppState
import Base

bindings =
  [ (Normal, ord 'i', return . insertMode)
  , (Normal, ord 'a', modifyCurrBuffer (return . moveRight Insert) . insertMode)
  , (Normal, ord 'h', modifyCurrBuffer (return . moveLeft))
  , (Normal, ord 'l', modifyCurrBuffer (return . moveRight Normal))
  , (Normal, ord 'k', modifyCurrBuffer (return . moveUp))
  , (Normal, ord 'j', modifyCurrBuffer (return . moveDown))
  , (Normal, ord 's', saveFile Nothing)
  , (Insert, 127, modifyCurrBuffer (return . removeChar))
  , (Insert, 10, modifyCurrBuffer (return . addLine))
  , (Insert, 27, modifyCurrBuffer (return . moveLeft) . normalMode)
  ] ++
  [(Insert, c, modifyCurrBuffer (return . addChar c)) |
      c <- [(ord ' ')..(ord '~')]]

addDefaultBindings :: AppState -> AppState
addDefaultBindings state =
  foldl (\s (mode, char, func) -> (addMappingFunc mode) char func s)
    state bindings
  where
    addMappingFunc Insert = addInsertMapping
    addMappingFunc Normal = addNormalMapping

handleChar :: CInt -> AppState -> IO AppState
handleChar cchar state@(AppState{stateMode = mode}) =
  case currMapping of
    Just func -> func state
    Nothing -> return state
  where
    char = fromIntegral cchar

    mapping Normal = stateNormalMapping state
    mapping Insert = stateInsertMapping state

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
  l <- gets $ bufferLines . getCurrBuffer
  (r,c) <- gets $ bufferPosition . getCurrBuffer
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
  let initState = makeState
  let bindingsState = addDefaultBindings initState
  args <- getArgs
  case args of
    [] -> runStateT loop bindingsState
    (file : _) -> do
      loadedState <- loadFile file bindingsState
      runStateT loop loadedState
  cursesCleanup
