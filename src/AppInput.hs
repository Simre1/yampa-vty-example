{-# LANGUAGE TemplateHaskell #-}

module AppInput where

import FRP.Yampa
import FRP.Yampa.Event (maybeToEvent)
import qualified Graphics.Vty.Input as I

-- This file handles input events from vty.

emptyAppInput = AppInput Nothing

data AppInput = AppInput
  { key :: Maybe I.Key
  } deriving Show


-- React to vty events and update AppInput
onEventInput :: AppInput -> I.Event -> AppInput
onEventInput ai (I.EvKey key _) = ai {key = Just key}
onEventInput ai _ = emptyAppInput


-- Signal functions to create events from AppInput
anyKeyEvent :: SF AppInput (Event I.Key)
anyKeyEvent = key ^>> arr maybeToEvent
