module TS.Utils where

import           Control.Arrow                  ( (***), first, second )
import           Control.Concurrent             ( threadDelay
                                                , killThread
                                                , forkIO
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock                ( utctDayTime
                                                , getCurrentTime
                                                )
import           Data.Time.LocalTime            ( todMin
                                                , timeToTimeOfDay
                                                )

-- | Yields the current minute of the current hour
minuteOfTheHour :: MonadIO m => m Int
minuteOfTheHour =
  liftIO $ todMin . timeToTimeOfDay . utctDayTime <$> getCurrentTime

-- | Blocks the current thread for `ms` minutes
waitMinutes :: MonadIO m => Int -> m ()
waitMinutes ms = liftIO $ threadDelay (ms * 60 * 1000000)

-- | Generates all lists of length `n`, conserving order
combinations :: Eq a => Int -> [a] -> [([a], [a])]
combinations 0 xs    = [([], xs)]
combinations _ [] = []
combinations n (x : xs') =
  (first (x :) <$> combinations (n - 1) xs') ++ (second (x :) <$> combinations n xs')
