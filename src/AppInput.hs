{-# LANGUAGE TemplateHaskell #-}

module AppInput where

import FRP.Yampa
import FRP.Yampa.Event (maybeToEvent)
import qualified Graphics.Vty.Input as I

data AppInput = AppInput
  { key :: Maybe I.Key
  } deriving Show

onEventInput :: AppInput -> I.Event -> AppInput
onEventInput ai (I.EvKey key _) = ai {key = Just key}
onEventInput ai _ = emptyAppInput

anyKeyEvent :: SF AppInput (Event I.Key)
anyKeyEvent = key ^>> arr maybeToEvent

emptyAppInput = AppInput Nothing
