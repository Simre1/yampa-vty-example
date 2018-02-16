{-# LANGUAGE TemplateHaskell #-}

module AppState where

import Graphics.Vty
import Lens.Micro.Platform

type ShouldQuit = Bool

data AppState = AppState
  { str :: String
  , shouldQuit :: Bool
  }

makeLenses ''AppState

renderAppState :: AppState -> (ShouldQuit, Picture)
renderAppState s =
  let image = string defAttr $ str s ++ ", press '#' to exit"
  in (shouldQuit s, picForImage image)
