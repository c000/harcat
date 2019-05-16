{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module  HarCat.Decode where

import Import
import Data.Aeson
import Data.ByteString.Base64
import Data.Maybe

import HarCat.Types

decodeFileRaw :: MonadIO m =>  FilePath -> m (Either String (Har Value))
decodeFileRaw path = liftIO $ eitherDecodeFileStrict path

decodeFileAnalized :: MonadIO m =>  FilePath -> m (Either String (Har Entry))
decodeFileAnalized path = liftIO $ eitherDecodeFileStrict path

entryToBytestring :: Entry -> Either Text ByteString
entryToBytestring e = case e ^. response . content . encoding of
  Nothing -> Right $ utf8Content
  Just "base64" -> Right . decodeLenient $ utf8Content
  Just enc -> Left $ "unrecogniszed encoding " <> enc
  where
    utf8Content = e ^. response . content . text . to fromJust . to encodeUtf8
