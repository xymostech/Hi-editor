module AppState where

import qualified Data.Map as Map

data Mode = Normal | Insert
  deriving Show

data AppState = AppState
  { stateLines :: [[Char]]
  , statePosition :: (Int, Int)
  , stateMode :: Mode
  , stateFilePath :: Maybe String
  , insertMapping :: Map.Map Int (AppState -> IO AppState)
  , normalMapping :: Map.Map Int (AppState -> IO AppState)
  }

makeState :: AppState
makeState = AppState
  { stateLines = [""]
  , statePosition = (0, 0)
  , stateMode = Normal
  , stateFilePath = Nothing
  , insertMapping = Map.empty
  , normalMapping = Map.empty
  }
