{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Facebook.Types where

import TextShow
import Utils
import Web.HttpApiData
import Data.Text (Text)
import Data.Aeson.TH
import Data.Monoid
import GHC.Generics
import Data.Hashable

-- API Types

data FBField =
      FBFId
    | FBFName
    | FBFBirthday
    | FBFFriends [FBField]
    deriving (Eq, Show, Generic)
instance Hashable FBField


instance TextShow FBField where
    showb FBFId          = "id"
    showb FBFName        = "name"
    showb FBFBirthday    = "birthday"
    showb (FBFFriends _) = "friends"

newtype FBFields = FBFields [FBField]
    deriving (Eq, Show, Generic)
instance Hashable FBFields

instance ToHttpApiData FBFields where
    toUrlPiece (FBFields [])     = ""
    toUrlPiece (FBFields (x:[])) = showt x
    toUrlPiece (FBFields ((FBFFriends fbs):xs)) = showt (FBFFriends []) <> "{" <> toUrlPiece (FBFields fbs) <> "}" <> "," <> toUrlPiece (FBFields xs)
    toUrlPiece (FBFields (x:xs)) = showt x <> "," <> toUrlPiece (FBFields xs)


data FBFriendsSummary = FBFriendsSummary
    { fbFriendsSummaryTotal_count :: Int
    } deriving (Show, Generic)
instance Hashable FBFriendsSummary


data FBFriends = FBFriends
    { fbFriendsData    :: [FBUser]
    , fbFriendsSummary :: FBFriendsSummary
    } deriving (Show, Generic)
instance Hashable FBFriends


data FBUser = FBUser
    { fbUserId      :: Text
    , fbUserName    :: Maybe Text
    , fbUserFriends :: Maybe FBFriends
    } deriving (Show, Generic)
instance Hashable FBUser


newtype Token = Token Text
    deriving (Eq, Show)

instance ToHttpApiData Token where
    toUrlPiece (Token txt) = txt


$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbFriendsSummary"}  ''FBFriendsSummary)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbFriends"}  ''FBFriends)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbUser"}  ''FBUser)
