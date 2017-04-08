{-# LANGUAGE ScopedTypeVariables #-}
module API.Server where

import Servant
import API.API
import Network.Wai.Handler.Warp
import System.Environment (getEnv)
import Control.Monad (liftM)

server :: IO ()
server = do
    port :: Int <- liftM read $ getEnv "PORT"
    run port (serve serverAPI api)
