module AppState where

import qualified Data.Map as Map
import Util

data Mode = Normal | Insert
  deriving Show

data Buffer = Buffer
  { bufferLines :: [String]
  , bufferPosition :: (Int, Int)
  }

data AppState = AppState
  { stateCurrBuffer :: Int
  , stateBuffers :: [Buffer]
  , stateMode :: Mode
  , stateFilePath :: Maybe String
  , stateInsertMapping :: Map.Map Int (AppState -> IO AppState)
  , stateNormalMapping :: Map.Map Int (AppState -> IO AppState)
  }

makeState :: AppState
makeState = AppState
  { stateCurrBuffer = 0
  , stateBuffers = [buff]
  , stateMode = Normal
  , stateFilePath = Nothing
  , stateInsertMapping = Map.empty
  , stateNormalMapping = Map.empty
  }
  where
    buff = Buffer
      { bufferLines = [""]
      , bufferPosition = (0, 0)
      }

modifyBuffer :: Int -> (Buffer -> IO Buffer) -> AppState -> IO AppState
modifyBuffer index func state@AppState{stateBuffers = buffers} = do
  newBuffer <- func buffer
  return state{stateBuffers = replacePosition index newBuffer buffers}
  where
    buffer = buffers !! index

modifyCurrBuffer :: (Buffer -> IO Buffer) -> AppState -> IO AppState
modifyCurrBuffer func state@AppState{stateCurrBuffer = curr} =
  modifyBuffer curr func state

getBuffer :: Int -> AppState -> Buffer
getBuffer index state@AppState{stateBuffers = buffers} = buffers !! index

getCurrBuffer :: AppState -> Buffer
getCurrBuffer state@AppState{stateCurrBuffer = curr} = getBuffer curr state
