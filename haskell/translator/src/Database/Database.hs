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

type Token = Text
type FBId = Text


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    fbId FBId
    fbToken Token
    deriving Show
|]



-- | Store the token. If we already have the user in the DB, then we replace the
-- old token with the new one provided
storeToken :: FBId -> Token -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
storeToken fbId token = do
    x <- liftM listToMaybe . select $
            from $ \u -> do
            where_ (u ^. UserFbId ==. val fbId)
            limit 1
            return u
    case x of
        Nothing -> void . insert $ User fbId token
        Just u  -> update $ \u -> do
                        set u [ UserFbToken =. val token ]
                        where_ (u ^. UserFbId ==. val fbId)

getToken :: FBId -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe Token)
getToken fbId = liftM (fmap unValue . listToMaybe) . select $
    from $ \u -> do
    where_ (u ^. UserFbId ==. val fbId)
    limit 1
    return (u ^. UserFbToken)
