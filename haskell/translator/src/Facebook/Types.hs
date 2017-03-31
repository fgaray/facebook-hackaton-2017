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
      FBId
    | FBName
    | FBBirthday
    deriving (Eq, Show, Generic)
instance Hashable FBField


instance TextShow FBField where
    showb FBId       = "id"
    showb FBName     = "name"
    showb FBBirthday = "birthday"

newtype FBFields = FBFields [FBField]
    deriving (Eq, Show, Generic)
instance Hashable FBFields

instance ToHttpApiData FBFields where
    toUrlPiece (FBFields [])     = ""
    toUrlPiece (FBFields (x:[])) = showt x
    toUrlPiece (FBFields (x:xs)) = showt x <> "," <> toUrlPiece (FBFields xs)


data FBUser = FBUser
    { fbUserId   :: Text
    , fbUserName :: Maybe Text
    } deriving (Show, Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = stripPrefixJSON "fbUser"}  ''FBUser)
instance Hashable FBUser


newtype Token = Token Text
    deriving (Eq, Show)

instance ToHttpApiData Token where
    toUrlPiece (Token txt) = txt

