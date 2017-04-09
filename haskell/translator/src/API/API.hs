{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module API.API where

import Utils
import Data.Aeson.TH
import Data.Aeson
import Data.Text (Text)
import Data.Proxy
import API.GPG
import Control.Monad.IO.Class
import API.Chat
import Control.Monad
import Facebook.API
import Facebook.Types
import qualified Data.Text as T

import Servant



type ServerAPI =
         "test" :> Get '[JSON] Text
    :<|> "register" :> Post '[JSON] Register
    :<|> "chat" :> "get_friends"  :> ReqBody '[JSON] (EncriptedJSON FBIdJSON) :> Get '[JSON] (Either Text (EncriptedJSON Text))
    :<|> "chat" :> "login" :> ReqBody '[JSON] (EncriptedJSON FBLoginParams) :> Get '[JSON] ()
    :<|> "chat" :> "chats" :> ReqBody '[JSON] (EncriptedJSON FBIdJSON) :> Get '[JSON] (Either Text (EncriptedJSON Text))
    :<|> "chat" :> "send" :> ReqBody '[JSON] (EncriptedJSON FBSendChatParams) :> Get '[JSON] (Either Text (EncriptedJSON Text))
    :<|> "news" :> ReqBody '[JSON] (EncriptedJSON FBToken) :> Get '[JSON] (Either Text (EncriptedJSON [FBNewsFeed]))

data Register = Register

data FBToken = FBToken
    { fbTokenToken :: Text
    }

data FBIdJSON = FBIdJSON
    { fbIdJSONFbId :: Text
    }

data FBLoginParams = FBLoginParams
    { fbLoginParamsFbId :: Text
    , fbLoginParamsEmail :: Text
    , fbLoginParamsPasswd :: Text
    }

data FBSendChatParams = FBSendChatParams
    { fbSendChatParamsFbId      :: Text
    , fbSendChatParamsMessage   :: Text
    , fbSendChatParamsThreadsId :: Text
    }


serverAPI :: Proxy ServerAPI
serverAPI = Proxy

api :: Server ServerAPI
api = test 
    :<|> register 
    :<|> requestFriends 
    :<|> loginChat' 
    :<|> getChatThreads'
    :<|> sendMessageChat'
    :<|> getNews


test = return "Hola mundo"

register = undefined


requestFriends (EncriptedJSON (FBIdJSON fbId)) =
    liftM (fmap EncriptedJSON) . liftIO $ getChatFriends fbId

loginChat' (EncriptedJSON (FBLoginParams{..})) =
    liftIO $ loginChat fbLoginParamsFbId fbLoginParamsEmail fbLoginParamsPasswd

getChatThreads' (EncriptedJSON (FBIdJSON fbId)) =
    liftM (fmap EncriptedJSON) . liftIO $ getChatThreads fbId

sendMessageChat' (EncriptedJSON (FBSendChatParams{..})) =
    liftM (fmap EncriptedJSON) . liftIO $ sendMessageChat fbSendChatParamsFbId fbSendChatParamsMessage fbSendChatParamsThreadsId

getNews (EncriptedJSON (FBToken token)) = do
     x <- liftIO $ run (getNewsFeed (Just (Token token)))
     case x of
        Left err -> return $ Left (T.pack $ show err)
        Right x' -> return . Right . EncriptedJSON . fbDataData $ x'


$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbIdJSON"}  ''FBIdJSON)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbLoginParams"}  ''FBLoginParams)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbSendChatParams"}  ''FBSendChatParams)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON ""}  ''Register)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbToken"}  ''FBToken)
