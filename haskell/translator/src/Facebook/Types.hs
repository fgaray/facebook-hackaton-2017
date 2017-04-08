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


data FBDataSummary = FBDataSummary
    { fbDataSummaryTotal_count :: Int
    } deriving (Show, Generic)
instance Hashable FBDataSummary

data FBCursors = FBCursors
    { fbCursorsBefore :: Text
    , fbCursorsAfter  :: Text
    } deriving (Show, Generic)
instance Hashable FBCursors


data FBPaging = FBPaging
    { fbPagingCursors :: Maybe FBCursors
    } deriving (Show, Generic)
instance Hashable FBPaging


data FBData a = FBData
    { fbDataData    :: [a]
    , fbDataSummary :: Maybe FBDataSummary
    , fbDataPaging  :: Maybe FBPaging
    } deriving (Show, Generic)
instance Hashable a => Hashable (FBData a)

data FBFriends = FBFriends
    { fbFriendsData    :: [FBUser]
    , fbFriendsSummary :: FBDataSummary
    } deriving (Show, Generic)
instance Hashable FBFriends

data FBNewsFeedFrom = FBNewsFeedFrom
    { fbNewsFeedFromName :: Text
    , fbNewsFeedFromCategory :: Text
    , fbNewsFeedFromId :: Text
    } deriving (Show, Generic)
instance Hashable FBNewsFeedFrom

data FBNewsFeed = FBNewsFeed
    { fbNewsFeedId :: Text
    , fbNewsFeedFrom :: Maybe FBNewsFeedFrom
    , fbNewsFeedStory :: Maybe Text
    , fbNewsFeedName :: Maybe Text
    , fbNewsFeedType :: Maybe Text
    , fbNewsFeedCreated_type :: Maybe Text
    } deriving (Show, Generic)
instance Hashable FBNewsFeed

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

data FBPost = FBPost
    { fbPostId          :: Text
    , fbPostCaption     :: Maybe Text
    , fbPostCreate_time  :: Maybe Text
    , fbPostDescription :: Maybe Text
    , fbPostStatus_type :: Maybe Text
    , fbPostCountries   :: Maybe [Text]
    , fbPostType        :: Maybe Text
    } deriving Show


{-status type-}
{-enum{mobile_status_update, created_note, added_photos, added_video,-}
{-shared_story, created_group, created_event, wall_post, app_created_story,-}
{-published_story, tagged_in_photo, approved_friend}-}

{-type-}
{-enum{link, status, photo, video, offer}-}

$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbCursors"}  ''FBCursors)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbPagin"}  ''FBPaging)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbDataSummary"}  ''FBDataSummary)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbData"}  ''FBData)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbFriends"}  ''FBFriends)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbUser"}  ''FBUser)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbPost"}  ''FBPost)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbNewsFeedFrom"}  ''FBNewsFeedFrom)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbNewsFeed"}  ''FBNewsFeed)
