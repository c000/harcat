{-# LANGUAGE NoImplicitPrelude #-}
module  HarCat.Decode where

import Import
import Data.Aeson

import HarCat.Types

decodeFileRaw :: MonadIO m =>  FilePath -> m (Either String (Har Value))
decodeFileRaw path = liftIO $ eitherDecodeFileStrict path

decodeFileAnalized :: MonadIO m =>  FilePath -> m (Either String (Har Entry))
decodeFileAnalized path = liftIO $ eitherDecodeFileStrict path
