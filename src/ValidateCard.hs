{-# LANGUAGE OverloadedStrings #-}

module ValidateCard where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Luhn (validateLunh)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Read (readMaybe)
import Web.Scotty

validateCard :: IO ()
validateCard = do
  inputTexts <- newIORef (1 :: Int, mempty :: Map Int Text)
  scotty 3000 $ do
    get "/" $ do
      (_, inputs) <- liftIO $ readIORef inputTexts
      html $
        renderHtml $
          H.html $
            H.body $ do
              H.h1 "Credit Card Validation"
              H.form H.! A.method "post" H.! A.action "/" $ do
                H.input H.! A.type_ "text" H.! A.name "number"
                H.input H.! A.type_ "submit" H.! A.value "Validate"
              H.table $
                for_ (M.toList inputs) $ \(i, numbers) ->
                  H.tr $ do
                    H.td (H.toHtml i)
                    H.td (H.text numbers)
    post "/" $ do
      number <- param "number"
      liftIO $
        modifyIORef inputTexts $
          \(i, inputs) ->
            (i + 1, M.insert i (format number (validateText number)) inputs)
      redirect "/"

data Result = InvalidCard | ValidCard | InvalidInput

validateText :: Text -> Result
validateText a = case readMaybe $ T.unpack a of
  Just myNumber -> if validateLunh myNumber then ValidCard else InvalidCard
  Nothing -> InvalidInput

format :: Text -> Result -> Text
format myNumber InvalidCard = myNumber <> " Invalid!"
format myNumber ValidCard = myNumber <> " Valid!"
format myNumber InvalidInput = myNumber <> " Invalid Input!"