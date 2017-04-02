{-# LANGUAGE OverloadedStrings #-}
-- | Pruebas unitarias de los fields enviados en peticiones como /me?fields=friends
-- Es importante que funcione bien dado que es clave para hacer peticiones sobre
-- los datos que el usuario tiene en Facebook
module FacebookFieldSpec where


import Test.Hspec
import Facebook.Types
import Web.HttpApiData

spec :: Spec
spec = do
    describe "fields" $ do
        it "serialize with , the fields" $ do
            toUrlPiece (FBFields [FBFName])        `shouldSatisfy` (=="name")
            toUrlPiece (FBFields [])               `shouldSatisfy` (=="")
            toUrlPiece (FBFields [FBFName, FBFId]) `shouldSatisfy` (=="name,id")
        it "serialize with , and {} the fields of friends" $ do
            toUrlPiece (FBFields [FBFName, FBFFriends [FBFName], FBFId]) `shouldSatisfy` (=="name,friends{name},id")
            toUrlPiece (FBFields [FBFName, FBFFriends [FBFName, FBFId], FBFId]) `shouldSatisfy` (=="name,friends{name,id},id")
