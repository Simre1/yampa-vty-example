{-# LANGUAGE FlexibleContexts #-}
{-# Language Arrows #-}

module Main where

import Graphics.Vty
import Graphics.Vty.Input
import qualified FRP.Yampa as Y
import Control.Concurrent
import Data.Time.Clock
import Control.Monad

import AppInput (onEventInput, emptyAppInput, AppInput)
import AppState (shouldExit, renderAppState, AppState)
import MainSF (mainSF)

-- Set the minimal FPS. The view will also update when an event happens.
minimalFPS = 1

main = do
  vty <- mkVty defaultConfig
  time <- getCurrentTime >>= newMVar
  Y.reactimate
    (return $ Y.NoEvent)
    (senseInput vty time)
    (renderOutput vty)
    (parseInput Y.>>> mainSF)
  shutdown vty

renderOutput :: Vty -> Bool -> AppState -> IO Bool
renderOutput vty changed s = do
  when changed $ do
    update vty $ renderAppState s
  return (shouldExit s)

-- The View will update when an event happens or when the maximumWaitTime (currently 1s) is reached
senseInput :: Vty -> MVar UTCTime -> Bool -> IO (Double, Maybe (Y.Event Event))
senseInput vty lastInteraction canBlock = do

  let maximumWaitTime = minimalFPS*1000000

  -- Wait for next event
  maybeEvent <- waitFor maximumWaitTime $ nextEvent vty

  -- calculate the time difference between two evaluations
  time <- getCurrentTime
  lastTime <- swapMVar lastInteraction time
  let diff = fromIntegral . fromEnum $ diffUTCTime time lastTime
  
  return (diff, Y.Event <$> maybeEvent)

-- Uses onEventInput from AppInput.hs to get the new AppInput
parseInput :: Y.SF (Y.Event Event) AppInput
parseInput = Y.accumHoldBy onEventInput emptyAppInput

-- Wait for a given time for an IO action to finish, giving Nothing back if it does not complete in time.
waitFor :: Int -> IO a -> IO (Maybe a)
waitFor delay action = do
    done <- newEmptyMVar
    threads <- start done
    event <- takeMVar done
    cleanUp threads
    return event
  where
    start done = do
        t1 <- forkIO $ action >>= \x -> putMVar done (return x)
        t2 <- forkIO $ threadDelay delay >> putMVar done Nothing
        return (t1, t2)
    cleanUp (t1, t2) = do
        killThread t1
        killThread t2
