{-# LANGUAGE OverloadedStrings #-}
module API.Chat where


import System.Process
import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import Database.Database
import Data.Monoid


loginChat :: Text -> Text -> Text -> IO ()
loginChat fbId email passwd = do
    appState <- liftM (last . lines) $ readProcess "nodejs" (map T.unpack ["fb-chat/login.js", email, passwd]) ""
    runDB $ storeChatState fbId (T.pack appState)


getChatFriends :: Text -> IO (Either Text Text)
getChatFriends fbId = do
    appState <- runDB $ getAppState fbId 
    case appState of
        Nothing -> return . Left $ "App State not found for ID " <> fbId
        Just state -> do
            friends <- liftM lines $ readProcess "nodejs" (map T.unpack ["fb-chat/friends.js", state]) ""
            return (Right . T.pack . last $ friends)


getChatThreads :: Text -> IO (Either Text Text)
getChatThreads fbId = do
    appState <- runDB $ getAppState fbId 
    case appState of
        Nothing -> return . Left $ "App State not found for ID " <> fbId
        Just state -> do
            friends <- readProcess "nodejs" (map T.unpack ["fb-chat/threads.js", state]) ""
            return (Right . T.pack $ friends)

sendMessageChat :: Text -> Text -> Text -> IO (Either Text Text)
sendMessageChat fbId message threadId = do
    appState <- runDB $ getAppState fbId 
    case appState of
        Nothing -> return . Left $ "App State not found for ID " <> fbId
        Just state -> do
            output <- readProcess "nodejs" (map T.unpack ["fb-chat/send_message.js", state, message, threadId]) ""
            return (Right . T.pack $ output)
