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

import Cursor.TextField

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print $ endState

data TuiState =
  TuiState
    { stateCursor :: TextFieldCursor
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
  path <- resolveFile' "example.txt"
  maybeContents <- forgivingAbsence $ T.readFile (fromAbsFile path)
  contents <- getNetworks
  let tfc = makeTextFieldCursor $ fromString  $ justNames contents
  pure TuiState {stateCursor = tfc}

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
      let mDo :: (TextFieldCursor -> Maybe TextFieldCursor) -> EventM n (Next TuiState)
          mDo func = do
            let tfc = stateCursor s
            let tfc' = fromMaybe tfc $ func tfc
            let s' = s {stateCursor = tfc'}
            continue s'
      in case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey KEsc [] -> halt s
        EvKey KUp [] -> mDo textFieldCursorSelectPrevLine
        EvKey (KChar 'k') [] -> mDo textFieldCursorSelectPrevLine
        EvKey (KChar 'j') [] -> mDo textFieldCursorSelectNextLine
        EvKey KDown [] -> mDo textFieldCursorSelectNextLine
        _ -> continue s
    _ -> continue s

getNetworks :: IO String
getNetworks =
  readProcess  "nmcli" ["connection"] []

connectToNetwork network =
  readProcessWithExitCode "nmcli" ["up", network] ""
