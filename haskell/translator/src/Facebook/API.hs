{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Modelo de la API de FB
--
--
-- Notas de Graph:
--  * Nodos: básicamente elementos como un usuario, una foto, una página, un comentario.
--  * Perímetros: las conexiones entre esos elementos, como fotos de páginas o comentarios de fotos.
--  * Campos: información sobre esos elementos, como el cumpleaños de una persona o el nombre de una página .
--
-- Cada nodo tiene un identificador único que se utiliza para acceder a este
-- mediante la API Graph. No documentamos específicamente la estructura o el
-- formato de ningún nodo o identificador de objeto porque es extremadamente
-- probable que cambie con el tiempo y las aplicaciones no deben hacer
-- suposiciones con base en la estructura actual.
--
-- Se hace una similitud entre nodos y arístas de un grafo: nodes y edges
--
--
--
-- Ejemplo de uso de estas funciones:
--
--  > user <- run (getUserData token fields)
--
--
module Facebook.API where

import Network.HTTP.Client (Manager, newManager)
import Servant.API
import Servant.Client
import Web.HttpApiData
import Data.Aeson.TH
import Data.Text (Text)
import Data.Proxy
import Data.Monoid
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Utils.Links
import Facebook.Types
import System.Environment (getEnv)
import qualified Data.Text as T
import Control.Monad (liftM)
import Data.Either (isRight)



-- | Base URL to the version 2.8 of the API
baseUrl :: BaseUrl
baseUrl = BaseUrl Https "graph.facebook.com" 443 "/v2.8"



-- Functions API

getUserData :: Maybe Token -> Maybe FBFields -> ClientM FBUser



-- API
getUserData = client fbAPI



-- | FB API
type FacebookAPI =
    QueryParam "access_token" Token :> "me" :> QueryParam "fields" FBFields :> Get '[JSON] FBUser


fbAPI :: Proxy FacebookAPI
fbAPI = Proxy



-- | API call in the IO Monad

run :: ClientM a -> IO (Either ServantError a)
run endpoint = do
    manager <- newManager tlsManagerSettings
    res <- runClientM endpoint (ClientEnv manager baseUrl)
    case res of
      Left err -> error $ show err
      Right x -> return . Right $ x

-- | Run a test with a token in the enviroment
-- >>> let f = flip getUserData
-- >>> liftM isRight $ runTest (f (Just (FBFields [FBFName, FBFFriends [FBFName]])))
-- True
--
runTest :: (Maybe Token -> ClientM a) -> IO (Either ServantError a)
runTest endpoint = do
    token <- liftM (Token . T.pack) . getEnv $ "TOKEN"
    manager <- newManager tlsManagerSettings
    res <- runClientM (endpoint (Just token)) (ClientEnv manager baseUrl)
    case res of
      Left err -> error $ show err
      Right x -> return . Right $ x
