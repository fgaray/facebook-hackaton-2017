{-# LANGUAGE OverloadedStrings #-}
module API.GPG where


import Data.Aeson


data EncriptedJSON a = EncriptedJSON
    { encriptedJSON :: a
    }

instance ToJSON a => ToJSON (EncriptedJSON a) where
    toJSON (EncriptedJSON x) = object ["encriptedJSON" .= x]


instance FromJSON a => FromJSON (EncriptedJSON a) where
    parseJSON (Object v) = EncriptedJSON <$> v .: "encriptedJSON"
