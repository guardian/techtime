{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module TS.Client where

import           Prelude                 hiding ( (/), concat )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.Reader           ( MonadReader
                                                , reader
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Aeson                     ( decode )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.List                      ( intersperse )
import           Data.Text                      ( Text
                                                , unpack
                                                , concat
                                                )
import           Network.HTTP.Client            ( Manager
                                                , parseRequest
                                                , httpLbs
                                                , httpNoBody
                                                , responseBody
                                                , defaultManagerSettings
                                                , newManager
                                                )
import           TS.Data

root, nextmap, whoami :: String
root = "http://10.249.16.173:14361/game/v1"
nextmap = root / "map"
whoami = "regis"

(/) :: String -> String -> String
a / b = a ++ "/" ++ b

-- | Fetch the next map
fetch :: (MonadReader Env m, MonadThrow m, MonadIO m) => m (Maybe Map)
fetch = decode <$> get nextmap

-- | Submit a path
submit :: (MonadReader Env m, MonadThrow m, MonadIO m) => Map -> m ()
submit (Map mid ps) =
  let url = root / "submit" / whoami / unpack mid / (unpack . concat $ intersperse "," ps')
      ps' = label <$> ps
  in  do
        req <- parseRequest url
        mng <- reader manager
        liftIO (httpNoBody req mng)
        pure ()

-- | Get request
get :: (MonadReader Env f, MonadThrow f, MonadIO f) => String -> f ByteString
get url = do
  req <- parseRequest url
  mng <- reader manager
  liftIO (responseBody <$> httpLbs req mng)

-- | Game configuration
newtype Env = Env { manager :: Manager }

-- | Create default HTTP settings
createEnv :: IO Env
createEnv = Env <$> newManager defaultManagerSettings
