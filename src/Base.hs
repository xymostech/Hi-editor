module Base where

import Data.Char
import qualified Data.Map as Map

import AppState
import Util

addChar :: Int -> Buffer -> Buffer
addChar char buffer@Buffer{bufferLines = lines, bufferPosition = (r,c)} =
  buffer{bufferLines = lines', bufferPosition = (r, c + 1)}
  where
    lines' = modifyPosition r (insertAtPosition c (chr char)) lines

removeChar :: Buffer -> Buffer
removeChar buffer@Buffer{bufferLines = lines, bufferPosition = (r,c)} =
  buffer{bufferLines = lines', bufferPosition = (r, max (c - 1) 0)}
  where
    lines' = modifyPosition r (removeAtPosition c) lines

addLine :: Buffer -> Buffer
addLine buffer@Buffer{bufferLines = lines, bufferPosition = (r,c)} =
  buffer{bufferLines = lines', bufferPosition = (r + 1, 0)}
  where
    lines' = insertAtPosition (r + 1) "" lines

moveLeft :: Buffer -> Buffer
moveLeft buffer@Buffer{bufferLines = lines, bufferPosition = (r,c)} =
  buffer{bufferPosition = (r, max (c-1) 0)}

moveRight :: Mode -> Buffer -> Buffer
moveRight Normal buffer@Buffer{bufferLines = lines, bufferPosition = (r,c)} =
  buffer{bufferPosition = (r, min (c+1) ((length (lines !! r)) - 1))}
moveRight Insert buffer@Buffer{bufferLines = lines, bufferPosition = (r,c)} =
  buffer{bufferPosition = (r, min (c+1) (length (lines !! r)))}

moveUp :: Buffer -> Buffer
moveUp buffer@Buffer{bufferPosition = (r, c)} =
  buffer{bufferPosition = (max (r-1) 0, c)}

moveDown :: Buffer -> Buffer
moveDown buffer@Buffer{bufferLines = lines, bufferPosition = (r,c)} =
  buffer{bufferPosition = (min (r+1) ((length lines) - 1), c)}

normalMode :: AppState -> AppState
normalMode state = state{stateMode = Normal}

insertMode :: AppState -> AppState
insertMode state = state{stateMode = Insert}

addInsertMapping :: Int -> (AppState -> IO AppState) -> AppState -> AppState
addInsertMapping char func state@AppState{stateInsertMapping = mapping} =
  state{stateInsertMapping = mapping'}
  where
    mapping' = Map.insert char func mapping

addNormalMapping :: Int -> (AppState -> IO AppState) -> AppState -> AppState
addNormalMapping char func state@AppState{stateNormalMapping = mapping} =
  state{stateNormalMapping = mapping'}
  where
    mapping' = Map.insert char func mapping

loadFile :: String -> AppState -> IO AppState
loadFile path state = do
  file <- readFile path
  modifyCurrBuffer (\b -> return b{bufferLines = lines file,
                                   bufferPosition = (0, 0)})
                   state{stateFilePath = Just path}

saveFile :: Maybe String -> AppState -> IO AppState
saveFile maybePath state@AppState{stateFilePath = currPath} =
  case (maybePath, currPath) of
    (Nothing, Nothing) -> return state
    (Just path, _) -> do
      writeFile path $ unlines lines
      return state{stateFilePath = Just path}
    (Nothing, Just path) -> do
      writeFile path $ unlines lines
      return state
  where
    lines = bufferLines $ getCurrBuffer state
