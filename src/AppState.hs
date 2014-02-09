module AppState where

import qualified Data.Map as Map

data Mode = Normal | Insert
  deriving Show

data AppState = AppState
  { stateLines :: [[Char]]
  , statePosition :: (Int, Int)
  , stateMode :: Mode
  , insertMapping :: Map.Map Int (AppState -> AppState)
  , normalMapping :: Map.Map Int (AppState -> AppState)
  }

makeState :: AppState
makeState = AppState
  { stateLines = [""]
  , statePosition = (0, 0)
  , stateMode = Normal
  , insertMapping = Map.empty
  , normalMapping = Map.empty
  }
