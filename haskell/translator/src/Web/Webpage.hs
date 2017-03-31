{-# LANGUAGE OverloadedStrings #-}
module Web.Webpage where

import Web.Scotty




server :: IO ()
server = scotty 3000 $ do
    get "/" $ do
        file "webpages/token_requests.html"
