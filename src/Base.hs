module Base where

import Data.Char
import qualified Data.Map as Map

import AppState
import Util

addChar :: Int -> AppState -> AppState
addChar char state@AppState{stateLines = lines, statePosition = (r,c)} =
  state{stateLines = lines', statePosition = (r, c + 1)}
  where
    lines' = modifyPosition r (insertAtPosition c (chr char)) lines

removeChar :: AppState -> AppState
removeChar state@AppState{stateLines = lines, statePosition = (r,c)} =
  state{stateLines = lines', statePosition = (r, max (c - 1) 0)}
  where
    lines' = modifyPosition r (removeAtPosition c) lines

addLine :: AppState -> AppState
addLine state@AppState{stateLines = lines, statePosition = (r,c)} =
  state{stateLines = lines', statePosition = (r + 1, 0)}
  where
    lines' = insertAtPosition (r + 1) "" lines

moveLeft :: AppState -> AppState
moveLeft state@AppState{stateLines = lines, statePosition = (r, c)} =
  state{statePosition = (r, max (c-1) 0)}

moveRight :: AppState -> AppState
moveRight state@AppState{stateLines = lines, statePosition = (r, c),
                         stateMode = Normal} =
  state{statePosition = (r, min (c+1) ((length (lines !! r)) - 1))}
moveRight state@AppState{stateLines = lines, statePosition = (r, c),
                         stateMode = Insert} =
  state{statePosition = (r, min (c+1) (length (lines !! r)))}

moveUp :: AppState -> AppState
moveUp state@AppState{stateLines = lines, statePosition = (r, c)} =
  state{statePosition = (max (r-1) 0, c)}

moveDown :: AppState -> AppState
moveDown state@AppState{stateLines = lines, statePosition = (r, c)} =
  state{statePosition = (min (r+1) ((length lines) - 1), c)}

normalMode :: AppState -> AppState
normalMode state = state{stateMode = Normal}

insertMode :: AppState -> AppState
insertMode state =
  state{stateMode = Insert}

addInsertMapping :: Int -> (AppState -> AppState) -> AppState -> AppState
addInsertMapping char func state@AppState{insertMapping = mapping} =
  state{insertMapping = mapping'}
  where
    mapping' = Map.insert char func mapping

addNormalMapping :: Int -> (AppState -> AppState) -> AppState -> AppState
addNormalMapping char func state@AppState{normalMapping = mapping} =
  state{normalMapping = mapping'}
  where
    mapping' = Map.insert char func mapping
