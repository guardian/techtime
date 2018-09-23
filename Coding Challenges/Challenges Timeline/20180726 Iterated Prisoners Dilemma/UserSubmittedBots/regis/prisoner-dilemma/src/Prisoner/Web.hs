{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Prisoner.Web
( players
, engage
, status
, opponent
, betray
, cooperate
, url
) where

-----------------------------------------------------------------------------------------------------------------------
import Prelude hiding ((/), concat)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, reader, ask)
import Data.Aeson
import Data.ByteString.Lazy (ByteString, concat, toStrict, fromStrict)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Functor ((<&>))
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager, method, parseRequest, httpLbs, httpNoBody, responseBody)
import Prisoner.Data
-----------------------------------------------------------------------------------------------------------------------

-- | Get the list of players. We will engage all of them until they
--   capitulate like Napoleon in 1812
players :: (MonadReader Env f, MonadThrow f, MonadIO f) => f [Player]
players = do
  liftIO $ putStrLn "Fetching the list of players..."
  Just ps <- get "players" <&> decode
  liftIO $ traverse print ps
  pure ps

-- | Start a new game, if possible, against `p`
engage :: (MonadReader Env f, MonadThrow f, MonadIO f) => Player -> f (Maybe Game)  
engage (Player p) = post ("start" / fromStrict (encodeUtf8 p)) <&> decode

-- | Fetch the current status of game `g`
status :: (MonadReader Env f, MonadThrow f, MonadIO f) => Game -> f Status
status (Game g) = get ("game-status" / fromStrict (encodeUtf8 g)) <&> decode >>= pure . maybe NewGame id

-- | Fetch the current moves made by your opponent in game `g`. Note that this action is designed to
--   silently fail, so that the strategy of the current player can fall back to a default move. This
--   is an easy way to write games like parser combinators, i.e. `move <|> fallbackmove`
opponent :: (MonadReader Env f, MonadThrow f, MonadIO f) => Game -> f [Move]
opponent g = status g >>= \(Ongoing p1 p2 _ (GM _ _ ((Player n1), (Player n2)) _)) -> do
  (Player me) <- reader whoami
  pure (if n1 == me then p2 else p1)

-- | Make a move in game `g`
betray, cooperate :: (MonadReader Env f, MonadThrow f, MonadIO f) => Game -> f ()
betray (Game g)    = post ("play" / fromStrict (encodeUtf8 g) / "betray") *> pure ()
cooperate (Game g) = post ("play" / fromStrict (encodeUtf8 g) / "cooperate") *> pure ()

(/) :: ByteString -> ByteString -> ByteString
a / b = concat [a, "/", b]

get :: (MonadReader Env f, MonadThrow f, MonadIO f) => ByteString -> f ByteString
get p = do
  fp <- url p
  req <- parseRequest fp
  mng <- reader manager
  liftIO (responseBody <$> httpLbs req mng)
  
post :: (MonadReader Env f, MonadThrow f, MonadIO f) => ByteString -> f ByteString
post p = do
  fp <- url p
  req <- parseRequest fp
  let preq = req { method = "POST" }
  mng <- reader manager
  liftIO (responseBody <$> httpLbs preq mng)

url :: MonadReader Env f => ByteString -> f String
url p = ask <&> \env -> unpack (root env / "game" / key env / p)