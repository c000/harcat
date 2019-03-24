{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import hiding (log)
import Control.Error
import Lens.Micro.Platform
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.IO
import Text.Mustache

import HarCat.Decode
import HarCat.Enumerator
import HarCat.Types

import ListFile
import Output

runCat :: FilePath -> RIO App (Either Utf8Builder ())
runCat listPath = do
  inputPath <- optionsInputFile . appOptions <$> ask
  outputPath <- optionsOutputFile . appOptions <$> ask

  listText <- case listPath of
    "-" -> do
      logDebug $ "list from stdin"
      liftIO $ getContents
    p -> do
      logDebug $ "list from " <> displayShow p
      liftIO $ T.fromStrict <$> T.readFile p

  parsed <- runExceptT $ do
    indexes <- tryRight . fmapL (display . T.unlines . map T.pack)
             $ decodeListFile listText
    logDebug $ "concat index " <> displayShow indexes

    har <- tryRight . fmapL (display . T.pack) =<< decodeFileAnalized inputPath

    bss <- forM indexes $ \i ->
      case har ^? log . entries . ix i of
        Nothing  -> throwE . display $ "index " <> tshow i <> " out of range"
        Just ent -> case entryToBytestring ent of
          Left e -> throwE . display $ e
          Right bs -> return bs

    return bss

  case parsed of
    Left e -> return $ Left e
    Right bss -> do
      withOutput outputPath $ \consumer -> do
        runEffect $ yield bss >-> P.concat >-> consumer
      return $ Right ()

runListing :: RIO App (Either Utf8Builder ())
runListing = runExceptT $ do
  inputPath <- optionsInputFile . appOptions <$> ask
  templateText <- optionsListTemplate . appOptions <$> ask
  template <- tryRight . fmapL displayShow $ compileMustacheText "ListTemplate" templateText
  har <- tryRight =<< fmapL displayShow <$> decodeFileRaw inputPath
  let es = har ^. log . entries
  forM_ (formatEntries template es) $ \t -> do
    unless (T.null t) $ do
      liftIO $ putStrLn t

run :: RIO App ()
run = do
  catList <- optionsCatList . appOptions <$> ask
  result <- case catList of
    Nothing -> runListing
    Just listFile -> runCat listFile
  case result of
    Right () -> return ()
    Left e -> logError e
