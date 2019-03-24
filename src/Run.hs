{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import hiding (log)
import Control.Error
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.IO
import Text.Mustache

import HarCat.Decode
import HarCat.Enumerator
import HarCat.Types

run :: RIO App ()
run = do
  inputPath <- optionsInputFile . appOptions <$> ask
  templateText <- optionsListTemplate . appOptions <$> ask
  result <- runExceptT $ do
    template <- tryRight . fmapL displayShow $ compileMustacheText "ListTemplate" templateText
    har <- tryRight =<< fmapL displayShow <$> decodeFileRaw inputPath
    let es = har ^. log . entries
    forM_ (formatEntries template es) $ \t -> do
      unless (T.null t) $ do
        liftIO $ putStrLn t
  case result of
    Right () -> return ()
    Left e -> logError e
