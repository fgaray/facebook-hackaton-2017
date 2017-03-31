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

import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Web.HttpApiData
import Data.Aeson.TH
import Data.Text (Text)
import Data.Proxy
import TextShow
import Data.Monoid
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Utils.Links
import Utils



-- | Base URL to the version 2.8 of the API
baseUrl :: BaseUrl
baseUrl = BaseUrl Https "graph.facebook.com" 443 "/v2.8"



-- API Types

data FBField =
      FBId
    | FBName
    | FBBirthday

instance TextShow FBField where
    showb FBId       = "id"
    showb FBName     = "name"
    showb FBBirthday = "birthday"

newtype FBFields = FBFields [FBField]

instance ToHttpApiData FBFields where
    toUrlPiece (FBFields [])     = ""
    toUrlPiece (FBFields (x:[])) = showt x
    toUrlPiece (FBFields (x:xs)) = showt x <> "," <> toUrlPiece (FBFields xs)


data FBUser = FBUser
    { fbUserId   :: Text
    , fbUserName :: Maybe Text
    } deriving Show
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbUser"}  ''FBUser)


newtype Token = Token Text

instance ToHttpApiData Token where
    toUrlPiece (Token txt) = txt



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
