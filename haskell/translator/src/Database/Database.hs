{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GADTs                      #-}
module Database.Database where


import Database.Persist hiding (Update, (==.), update, (=.))
import Database.Persist.Sqlite hiding (Update, (==.), update, (=.))
import Database.Persist.TH
import Data.Text
import Database.Esqueleto
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Data.Maybe

type ChatState = Text
type FBId = Text


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    fbId FBId
    fbChatState ChatState
    deriving Show
|]



-- | Store the state. If we already have the user in the DB, then we replace the
-- old token with the new one provided
storeChatState :: FBId -> ChatState -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
storeChatState fbId state = do
    x <- liftM listToMaybe . select $
            from $ \u -> do
            where_ (u ^. UserFbChatState ==. val fbId)
            limit 1
            return u
    case x of
        Nothing -> void . insert $ User fbId state
        Just u  -> update $ \u -> do
                        set u [ UserFbChatState =. val state ]
                        where_ (u ^. UserFbId ==. val fbId)

getAppState :: FBId -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe ChatState)
getAppState fbId = liftM (fmap unValue . listToMaybe) . select $
    from $ \u -> do
    where_ (u ^. UserFbId ==. val fbId)
    limit 1
    return (u ^. UserFbChatState)



runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDB sql = runSqlite "db.sqlite3" (runMigration migrateAll >> sql)
    
