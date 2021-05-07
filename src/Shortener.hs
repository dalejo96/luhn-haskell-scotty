{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as LT
import Luhn (validateLunh)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Read (readMaybe)
import Web.Scotty

shortener :: IO ()
shortener = do
  urlsR <- newIORef (1 :: Int, mempty :: Map Int Text)
  scotty 3000 $ do
    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlsR
      html $
        renderHtml $
          H.html $
            H.body $ do
              H.h1 "Credit Card Validation"
              H.form H.! A.method "post" H.! A.action "/" $ do
                H.input H.! A.type_ "text" H.! A.name "number"
                H.input H.! A.type_ "submit" H.! A.value "Validate"
              H.table $
                for_ (M.toList urls) $ \(i, url) ->
                  H.tr $ do
                    H.td (H.toHtml i)
                    H.td (H.text url)
    post "/" $ do
      number <- param "number"
      liftIO $
        modifyIORef urlsR $
          \(i, urls) ->
            (i + 1, M.insert i (format number (validateText number)) urls)
      redirect "/"
    get "/:n" $ do
      n <- param "n"
      (_, urls) <- liftIO $ readIORef urlsR
      case M.lookup n urls of
        Just url ->
          redirect (LT.fromStrict url)
        Nothing ->
          raise "not found"

validateText :: Text -> Bool
validateText a = case readMaybe $ unpack a of
  Just myNumber -> validateLunh myNumber
  Nothing -> False

--maybe False validateLunh (readMaybe $ unpack a)

format :: Text -> Bool -> Text
format myNumber False = myNumber <> "Invalid!"
format myNumber True = myNumber <> "Valid!"

---number Valid!
---number Invalid!