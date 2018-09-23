{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Arrow                  ( (&&&) )
import           Control.Concurrent             ( killThread
                                                , forkIO
                                                )
import           Control.Monad                  ( forever
                                                , (>=>) 
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Reader           ( runReaderT )
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                , atomicModifyIORef'
                                                )
import           Data.List                      ( permutations )
import           Data.List.NonEmpty as NL       ( fromList )
import           Data.Semigroup                 ( sconcat
                                                , Min(..)
                                                )
import           TS.Client
import           TS.Data
import           TS.Utils

-- | The program is an infinite loops working as follows:
-- - at xx:01, the next map of points is fetched
-- - ~1K threads are spawn to each compute the best path starting with a prefix
--   made of 6 distinct elements out of the list
-- - at xx:59, all the threads are killed if they haven't died already and
--   the best result so far is submitted to the server
main :: IO ()
main = createEnv >>= forever . runReaderT loop
  where
    loop = do
      maybeWait
      Map mid points <- fetch
      liftIO $ putStrLn $ "Ok here we go with map " ++ show mid ++ "..."
      ref  <- liftIO $ newIORef (points, cost points)
      tids <- liftIO $ traverse (forkIO . run ref) (combinations 6 points)
      wait59
      liftIO $ traverse killThread tids
      (points', cost') <- liftIO $ readIORef ref
      liftIO
        $  putStrLn
        $  "Submit our best result "
        ++ show points'
        ++ " ("
        ++ show cost'
        ++ ")"
      submit (Map mid points')

maybeWait :: MonadIO m => m ()
maybeWait = do
  mod <- minuteOfTheHour
  let wait = (60 - mod + 1)
  if mod > 50
    then do
      liftIO
        $  putStrLn
        $  "It's "
        ++ show mod
        ++ " minutes past the hour, waiting "
        ++ show wait
        ++ "..."
      waitMinutes wait
    else pure ()

-- | Sleep until 59 past the hour
wait59 :: MonadIO m => m ()
wait59 = (59-) <$> minuteOfTheHour >>= waitMinutes

-- | We receive two subsequences and compute their respective minimum path
--   and submit the concatenation of both as our result. This won't
--   always yield the best path but I'm fine with that.
run :: MonadIO m => IORef ([Point], Double) -> ([Point], [Point]) -> m ()
run ref (prefix, xs) = liftIO $ do
  (_, cost) <- readIORef ref
  if cost' < cost
    then (putStrLn $ "I'm done with " ++ show challenger) *> atomicModifyIORef' ref (update challenger)
    else putStrLn "Nothing good from me, buh-bye!"
  where
    challenger@(_, cost') = let xs = minprefix <> minsuffix in (xs, cost xs)
    minprefix = compute prefix
    minsuffix = compute xs
    update p @ (_, cost) p' @ (_, cost') = if cost < cost' then (p, ()) else (p', ())
    compute = fst . getChallenger . getMin . sconcat . NL.fromList . map (Min . Challenger . (id &&& cost)) . permutations

