{-# LANGUAGE TemplateHaskell #-}

module AppInput where

import FRP.Yampa
import FRP.Yampa.Event (maybeToEvent)
import qualified Graphics.Vty.Input as I
import Lens.Micro.Platform

data AppInput = AppInput
  { key :: Maybe I.Key
  } deriving Show

makeLenses ''AppInput

onEventInput :: AppInput -> I.Event -> AppInput
onEventInput ai (I.EvKey key _) = ai {key = Just key}
onEventInput ai _ = emptyAppInput

anyKeyEvent :: SF AppInput (Event I.Key)
anyKeyEvent = key ^>> arr maybeToEvent

-- quitEvent :: SF AppInput (Event ())
-- quitEvent = quit ^>> edgeJust

emptyAppInput = AppInput Nothing
