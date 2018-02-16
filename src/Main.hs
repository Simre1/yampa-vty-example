{-# LANGUAGE FlexibleContexts #-}
{-# Language Arrows #-}

module Main where

import Graphics.Vty
import Graphics.Vty.Input
import qualified FRP.Yampa as Y
import Control.Concurrent
import Data.Time.Clock
import Control.Monad

import AppInput
import AppState
import MainSF (mainSF)
import Lens.Micro.Platform

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
    update vty $ snd $ renderAppState s
  return (shouldQuit s)


senseInput :: Vty -> MVar UTCTime -> Bool -> IO (Double, Maybe (Y.Event Event))
senseInput vty lastInteraction canBlock = do
  let cycleTime = 1000000
  maybeEvent <- waitFor cycleTime $ nextEvent vty
  time <- getCurrentTime
  lastTime <- swapMVar lastInteraction time
  let diff = fromIntegral . fromEnum $ diffUTCTime time lastTime
  return (diff, Y.Event <$> maybeEvent)

parseInput :: Y.SF (Y.Event Event) AppInput
parseInput = Y.accumHoldBy onEventInput emptyAppInput

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
