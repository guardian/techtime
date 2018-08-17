{-# LANGUAGE OverloadedStrings, RankNTypes, DeriveGeneric #-}

module Prisoner.Data
( Move(..)
, Round
, Strategy
, Player(..)
, Game(..)
, GameMetadata(..)
, Status(..)
, Env(..)
, History(..)
, optimist
, nasty
, titfortat
, pavlov
) where

-----------------------------------------------------------------------------------------------------------------------
import Control.Applicative ((<|>))
import Control.Monad (MonadPlus, forever)
import Control.Monad.State.Lazy (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import Data.Scientific (coefficient, base10Exponent)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Network.HTTP.Client (Manager)
-----------------------------------------------------------------------------------------------------------------------

data Move = Cooperate | Betray deriving Eq

instance FromJSON Move where
  parseJSON = withScientific "Move" readMove
    where readMove n | c == 0 && e == 0 = pure Betray
                     | c == 1 && e == 0 = pure Cooperate
                     | otherwise        = fail ("A move can only be the number 0 (betray) or 1 (cooperate), got " ++ show n)
                     where c = coefficient n
                           e = base10Exponent n

type Round = (Move, Move)

-- | A player is represented by its strategy. It may need to know what moves the
--   opponent has played so far to decide what do to next; "may" because some
--   strategies are eager and will just ignore the opponent. That's why we need
--   to capture everything in a functor.
--   Heavily inspired from Bird's "Thinking functionally with Haskell"
type Strategy f = f [Move] -> f Move

-- A move is successful if your opponent cooperates
wins :: Move -> Move -> Bool
Betray    `wins` Cooperate = True
Cooperate `wins` Cooperate = True
_         `wins` _         = False

stay, switch :: Move -> Move
stay             = id
switch Cooperate = Betray
switch Betray    = Cooperate

-- win-stay lose-switch for the Pavlov strategy
wsls :: Move -> Move -> Move
wsls m n = if m `wins` n then stay m else switch m

-- TODO: devious, random, probabilistic
optimist, nasty, titfortat, pavlov :: (MonadPlus f) => Strategy f
optimist _    = forever (pure Cooperate)            -- always cooperate
nasty _       = forever (pure Betray)               -- always betray
titfortat ms  = (head <$> ms) <|> pure Cooperate    -- cooperate then copy
pavlov ms     = evalStateT fsm Cooperate            -- cooperate then win-stay lose-switch
  where fsm = next <|> get
        next = do m <- get
                  m' <- wsls m . head <$> lift ms
                  if m == m'
                    then pure m
                    else put m' *> pure m'

  
newtype Player = Player Text deriving (Eq, Ord, Generic, Show)

instance FromJSON Player

newtype Game = Game Text deriving (Generic)

instance FromJSON Game

data GameMetadata = GM Game UTCTime (Player, Player) Text

instance FromJSON GameMetadata where
  parseJSON = withObject "GameMetadata" $ \v -> GM
    <$> v .: "game_id"
    <*> v .: "starting_date"
    <*> v .: "players"
    <*> v .: "status"

data Status = NewGame
            | Ongoing { player1  :: [Move]
                      , player2  :: [Move]
                      , scores   :: (Integer, Integer)
                      , metadata :: GameMetadata
                      }

-- | The `NewGame` inhabitant is only a local necessity, as the server only
--   provides data for ongoing games. This is "proof by JSON decoding" :-D
instance FromJSON Status where
  parseJSON = withObject "Status" $ \v ->
    v .: "game_metadata" >>= \gm@(GM _ _ ((Player p1), (Player p2)) _) -> Ongoing
      <$> v .: p1
      <*> v .: p2
      <*> v .: "scores"
      <*> pure gm

-- Game configuration
data Env = Env { manager :: Manager 
               , key     :: ByteString
               , root    :: ByteString
               , whoami  :: Player
               }

-- Keep track of each game, especially those betraying bastards!!!
data History = History (M.Map Player [(Int, Int)])