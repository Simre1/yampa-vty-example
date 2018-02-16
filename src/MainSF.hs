{-# Language Arrows #-}

module MainSF
  (mainSF) where

import FRP.Yampa
import qualified Graphics.Vty.Input as I

import AppInput
import AppState


mainSF :: SF AppInput AppState
mainSF = proc input -> do
  keyEvent <- tagWith () ^<< filterE (==I.KChar '#') ^<< anyKeyEvent -< input
  let shouldQuit = isEvent keyEvent
  returnA -< AppState "Hello World" shouldQuit
