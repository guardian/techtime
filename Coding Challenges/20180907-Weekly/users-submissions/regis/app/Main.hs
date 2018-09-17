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

main :: IO ()
main = createEnv >>= forever . runReaderT loop
  where
    loop = do
      maybeWait
      Just m@(Map mid ps) <- fetch
      liftIO $ putStrLn $ "Ok here we go with map " ++ show mid ++ "..."
      ref  <- liftIO $ newIORef (ps, cost ps)
      tids <- liftIO $ traverse (forkIO . run ref) (listsOfN 4 ps)
      wait58
      liftIO $ traverse killThread tids
      (ps, t) <- liftIO $ readIORef ref
      liftIO
        $  putStrLn
        $  "Submit our best result "
        ++ show ps
        ++ " ("
        ++ show t
        ++ ")"
      submit (Map mid ps)

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

-- | Sleep until 58 past the hour
wait58 :: MonadIO m => m ()
wait58 = (58-) <$> minuteOfTheHour >>= waitMinutes

run :: MonadIO m => IORef ([Point], Double) -> ([Point], [Point]) -> m ()
run ref (prefix, xs) = liftIO $ do
  (_, cw) <- readIORef ref
  if snd challenger < cw
    then (putStrLn $ "I'm done with " ++ show challenger) *> atomicModifyIORef' ref (update challenger)
    else putStrLn "Nothing good from me, buh-bye!"
  where
    challenger = let xs = minprefix <> minsuffix in (xs, cost xs)
    minprefix = compute prefix
    minsuffix = compute xs
    update p @ (_, cc) p' @ (_, cw) = if cc < cw then (p, ()) else (p', ())
    compute = fst . getChallenger . getMin . sconcat . NL.fromList . map (Min . Challenger . (id &&& cost)) . permutations

