module Main where

import UI.HSCurses.Curses
import Control.Concurrent
import Foreign.C.Types
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Char
import Prelude hiding (lines)

import AppState
import Base

isPrintableCharacter :: Int -> Bool
isPrintableCharacter char =
  (char >= space && char <= tilde)
  where
    space = ord ' '
    tilde = ord '~'

handleChar :: CInt -> AppState -> AppState
handleChar cchar state@(AppState{stateMode = Insert})
  | isPrintableCharacter char = addChar char state
  | char == 127 = removeChar state
  | char == 10 = addLine state
  | char == 27 = moveLeft . normalMode $ state
  | otherwise = state
  where
    char = fromIntegral cchar
handleChar cchar state@(AppState{stateMode = Normal})
  | char == ord 'a' = moveRight . insertMode $ state
  | char == ord 'i' = insertMode state
  | char == ord 'h' = moveLeft state
  | char == ord 'l' = moveRight state
  | char == ord 'k' = moveUp state
  | char == ord 'j' = moveDown state
  | otherwise = state
  where
    char = fromIntegral cchar

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
  modify $ handleChar key
  when (key /= 3) loop

main :: IO ()
main = do
  cursesSetup
  runStateT loop makeState
  cursesCleanup
