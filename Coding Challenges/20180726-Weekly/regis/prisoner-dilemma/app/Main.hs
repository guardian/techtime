{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

-----------------------------------------------------------------------------------------------------------------------
import Control.Monad (forever)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runStateT)
import Data.ByteString.Lazy (ByteString)
import Data.Map (empty)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Prisoner
-----------------------------------------------------------------------------------------------------------------------

me :: Player
me = Player "Regis.Kuckaertz"

apiKey, baseUrl :: ByteString
apiKey  = "1234"
baseUrl = "http://localhost"
    
main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let env = Env manager apiKey baseUrl me
  forever $ runReaderT (runStateT prisoner (History empty)) env 
