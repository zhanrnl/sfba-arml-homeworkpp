{-# LANGUAGE OverloadedStrings #-}

module Controllers where

import Snap.Core
import Control.Applicative
import Data.Maybe
import Snap.Snaplet
import Snap.Snaplet.Session
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Utf8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding
import Control.Monad.Trans
import Data.Aeson ((.=))
import qualified Data.Aeson as AE
import Data.Monoid ((<>))
import Data.Time
import qualified Data.Map as M

import qualified Views as V
import Types
import Models


render :: Html -> AppHandler ()
render = writeLBS . renderHtml

requireAuth :: AppHandler () -> AppHandler ()
requireAuth handler = do
  maybeUsername <- getSessUsername
  case maybeUsername of
    Nothing -> redirect "/login"
    Just _ -> handler

requireNotAuth :: AppHandler () -> AppHandler ()
requireNotAuth handler = do
  maybeUsername <- getSessUsername
  case maybeUsername of
    Nothing -> handler
    Just _ -> redirect "/"

getSessUsername :: AppHandler (Maybe Text)
getSessUsername = with sess $ getFromSession "username"

getSessJustUsername :: AppHandler Text
getSessJustUsername = maybe "" id <$> getSessUsername

getTextParam :: B.ByteString -> AppHandler Text
getTextParam paramName = decodeUtf8 . (maybe "" id) <$> getParam paramName

-- CONTROLLERS

index :: AppHandler ()
index = requireAuth $ do
  self <- getSessJustUsername
  render $ V.index self

notFound :: AppHandler ()
notFound = do
  getResponse >>= (putResponse . setResponseCode 404)
  render V.notFound