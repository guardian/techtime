{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Prisoner
( module Prisoner.Data
, module Prisoner.Web
, match
, score
, total
, prisoner
) where

-----------------------------------------------------------------------------------------------------------------------
import Control.Arrow ((&&&))
import Control.Monad (MonadPlus, replicateM)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState, get, modify)
import Data.Functor ((<&>))
import Data.List (foldl1', sortOn)
import Data.Map ((!?), adjust)
import Data.Ord (Down(..))
import Prisoner.Data
import Prisoner.Web
import System.Timeout (timeout)
-----------------------------------------------------------------------------------------------------------------------

fiveminutes :: Int
fiveminutes = 5 * 60 * 10^6

-- | A match between two players is made of n rounds:
--   1. we move first, using the opponent's previous moves to decide if available, then wait for the opponent's next move
--   2. the score of each round is calculated
--   3. the overall score for the match is produced for bookkeeping
match :: (MonadIO f, MonadPlus f, MonadThrow f, MonadReader Env f) => Int -> Game -> Strategy f -> f (Int, Int)
match n game player = replicateM n play <&> map score <&> total
  where play = do m <- player (opponent game)
                  case m of
                    Betray    -> betray game
                    Cooperate -> cooperate game
                  (m' : _) <- opponent game
                  pure (m, m')

score :: Round -> (Int, Int)
score (Cooperate, Cooperate) = (3, 3)
score (Cooperate, Betray)    = (0, 5)
score (Betray,    Cooperate) = (5, 0)
score (Betray,    Betray)    = (1, 1)
                  
total :: [(Int, Int)] -> (Int, Int)
total = foldl1' (\(x, y) (x', y') -> (x+x', y+y'))

-- | trivial selection strategy: we choose the player against which
--   we have been the most successful so far
pick :: MonadState History f => [Player] -> f Player
pick ps = do
  (History games) <- get
  let ps' = sortOn (Down . snd) . map (id &&& (maybe 0 summary . (games !?))) $ ps
  pure $ fst $ head ps'

-- | overall score for all games played with a particular opponent
summary :: [(Int, Int)] -> Int
summary = foldr ((+) . uncurry (-)) 0

-- | This is the full game.
--   1. get the list of all players
--   2. look into the past and pick an opponent
--   3. try to engage, if unsuccesful go back to 1
--   4. play against the opponent
--   5. record result
prisoner :: (MonadState History f, MonadReader Env f, MonadThrow f, MonadPlus f, MonadIO f) => f ()
prisoner = players >>= pick >>= \p -> do
  engage p >>= \case
    Nothing -> prisoner
    Just gid -> do
      res <- match 10 gid pavlov
      _ <- update p res
      pure ()
  where
    update p r = modify (\(History rs) -> History (adjust (r:) p rs))