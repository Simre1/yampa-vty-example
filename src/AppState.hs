{-# LANGUAGE TemplateHaskell #-}

module AppState where

import Graphics.Vty

data AppState = AppState
  { str :: String
  , shouldQuit :: Bool
  }

renderAppState :: AppState -> Picture
renderAppState s =
  let image = string defAttr $ str s ++ ", press '#' to exit"
  in picForImage image
