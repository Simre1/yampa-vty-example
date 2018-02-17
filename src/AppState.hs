{-# LANGUAGE TemplateHaskell #-}

module AppState where

import Graphics.Vty

-- This file defines the whole AppState and how it is rendered in the terminal

data AppState = AppState
  { str :: String
  , shouldQuit :: Bool
  }

renderAppState :: AppState -> Picture
renderAppState s =
  let image = string defAttr $ str s ++ ", press '#' to exit"
  in picForImage image

-- This function is used in the mainloop to check if yampa should exit.
shouldExit :: AppState -> Bool
shouldExit = shouldQuit
