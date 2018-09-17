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
listsOfN :: Eq a => Int -> [a] -> [([a], [a])]
listsOfN 0 xs    = [([], xs)]
listsOfN _ [] = []
listsOfN n (x : xs') =
  (first (x :) <$> listsOfN (n - 1) xs') ++ (second (x :) <$> listsOfN n xs')
