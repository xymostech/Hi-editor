module AppState where

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
