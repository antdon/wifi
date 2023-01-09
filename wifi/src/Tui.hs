{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Directory

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Maybe
import Data.String
import qualified Data.Text.IO as T
import Graphics.Vty.Input.Events
import Path
import Path.IO
import Text.Show.Pretty
import System.Process
import Cursor.Brick.TextField
import Parser
import Control.Monad.IO.Class

import Cursor.TextField
import Text.Printf (printf)
import GHC.Real (Integral)
import Data.Maybe (Maybe(Nothing))

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print $ endState

data TuiState =
  TuiState
    { stateCursor :: TextFieldCursor,
      listIndex :: Int
    }
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: IO TuiState
buildInitialState = do
  contents <- getNetworks
  rawCurrentConnected <- getCurrentConnection
  let tfc = makeTextFieldCursor $ fromString  $ stringify $ ("Current Connection: " ++ fifthWord rawCurrentConnected) : justNames contents
  pure TuiState {stateCursor = tfc, listIndex = 0}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [ centerLayer $
             border $
             padLeftRight 1 $
             selectedTextFieldCursorWidget ResourceName (stateCursor ts)
             ]

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      let mDo :: (TextFieldCursor -> Maybe TextFieldCursor) -> Int -> EventM n (Next TuiState)
          mDo func change = do
            let tfc = stateCursor s
            let index = listIndex s
            let tfc' = fromMaybe tfc $ func tfc
            let index' = index + change
            let sChange = s {stateCursor = tfc', listIndex = index'}
            let sRemain = s {stateCursor = tfc', listIndex = index}
            if' (func tfc == Nothing) (continue sRemain) (continue sChange)
      in case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KEsc [] -> halt s
        EvKey KUp [] -> mDo textFieldCursorSelectPrevLine (-1)
        EvKey (KChar 'k') [] -> mDo textFieldCursorSelectPrevLine (-1)
        EvKey (KChar 'j') [] -> mDo textFieldCursorSelectNextLine 1
        EvKey KDown [] -> mDo textFieldCursorSelectNextLine 1
        EvKey KEnter [] -> handleEnter s
        _ -> continue s
    _ -> continue s

getNetworks :: IO String
getNetworks =
  readProcess  "nmcli" ["connection"] []

connectToNetwork network =
  readProcessWithExitCode "nmcli" ["connection", "up", network] ""

-- returns the new textFieldCursor with a updated currently connected network
connectSelected state = do
  contents <- getNetworks
  rawCurrentConnected <- getCurrentConnection
  let names = justNames $ contents
  let index = listIndex state
  let network = indexify names [0,1..index-1]
  connectToNetwork network
  let tfc = makeTextFieldCursor $ fromString  $ stringify $ ("Current Connection: " ++ fifthWord rawCurrentConnected) : justNames contents
  pure TuiState {stateCursor = tfc, listIndex = index-1}

handleEnter :: TuiState -> EventM n (Next TuiState)
handleEnter s = do
  liftIO $ connectSelected s
  continue s

indexify :: Num b => [a] -> [b] -> a
indexify (a:as) (b:bs) = indexify as bs
indexify a b = head a

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- gets the name of the network currently connected to
getCurrentConnection :: IO String
getCurrentConnection =
  readProcess "nmcli" ["connection", "show", "--active"] []
  

  
