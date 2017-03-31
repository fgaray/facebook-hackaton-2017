{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Peticiones para la API de FB Graph
module Facebook.Requests where


import Prelude hiding (mapM_, mapM)
import Haxl.Core hiding (fetchReq)
import Haxl.Prelude
import Data.Typeable
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Exception
import Control.Concurrent.QSem
import Control.Concurrent.Async
import Servant.Client
import GHC.Generics
import Data.Hashable

import qualified Facebook.API as API
import Facebook.Types


data FacebookReq a where
    GetUserData :: FBFields -> FacebookReq FBUser
    deriving Typeable

instance Hashable a => Hashable (FacebookReq a) where
    hashWithSalt n (GetUserData x) = hashWithSalt n x

deriving instance Eq (FacebookReq a)
deriving instance Show (FacebookReq a)

instance ShowP FacebookReq where
    showp _ = "FacebookReq"


instance DataSourceName FacebookReq where
    dataSourceName _ = "Facebook"

instance DataSource u FacebookReq where
    fetch = facebookFetch

    


instance StateKey FacebookReq where
    data State FacebookReq = FacebookState
        { manager         :: Manager
        , numThreads      :: Int
        , userAccessToken :: Token
        }

initGlobalState :: Int -> Token -> IO (State FacebookReq)
initGlobalState threads token = do
    manager <- newManager tlsManagerSettings
    return $ FacebookState
        { manager = manager
        , numThreads = threads
        , userAccessToken = token
        }



facebookFetch
    :: State FacebookReq 
    -> Flags 
    -> u 
    -> [BlockedFetch FacebookReq]
    -> PerformFetch
facebookFetch FacebookState{..} flags user bfs =
    AsyncFetch $ \inner -> do
        sem <- newQSem numThreads
        asyncs <- mapM (fetchAsync manager userAccessToken sem) bfs
        inner
        mapM_ wait asyncs


data ServantException = ServantException String
    deriving (Show, Typeable)
instance Exception ServantException


fetchAsync
    :: Manager 
    -> Token 
    -> QSem
    -> BlockedFetch FacebookReq
    -> IO (Async ())
fetchAsync manager token sem (BlockedFetch req rvar) =
    async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
        e <- Control.Exception.try $ runClientM  (fetchReq manager token req) (ClientEnv manager API.baseUrl) 
        case e of
            Left ex -> putFailure rvar (ex :: SomeException)
            Right a -> case a of
                Left err -> putFailure rvar (ServantException (show err))
                Right x -> putSuccess rvar x

fetchReq :: Manager -> Token -> FacebookReq a -> ClientM a
fetchReq manager t req =
    let token = Just t
    in case req of
        GetUserData fields -> API.getUserData token (Just fields)




getUserData :: FBFields -> GenHaxl u FBUser
getUserData fields = dataFetch (GetUserData fields)






testHaxl :: IO ()
testHaxl = do
    let token = Token ""
    state <- initGlobalState 5 token
    env <- initEnv (stateSet state stateEmpty) ()
    r <- runHaxl env $ do
        getUserData (FBFields [FBName])
    print r
