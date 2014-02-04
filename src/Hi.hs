import UI.HSCurses.Curses
import Control.Concurrent
import Foreign.C.Types
import Data.Char
import Control.Monad

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

isPrintableCharacter :: CInt -> Bool
isPrintableCharacter cchar =
  (char >= a && char <= z) || (char >= capA && char <= capZ)
  where
    char = (fromIntegral cchar)
    a = ord 'a'
    z = ord 'z'
    capA = ord 'A'
    capZ = ord 'Z'

loop :: IO ()
loop = do
  update
  key <- getch
  when (isPrintableCharacter key) $ (waddch stdScr (fromIntegral key)) >> loop

main :: IO ()
main = do
  cursesSetup
  loop
  cursesCleanup
