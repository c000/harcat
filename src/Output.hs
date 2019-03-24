{-# LANGUAGE NoImplicitPrelude #-}
module Output where

import Import hiding (stdout)
import RIO.File

import Pipes
import Pipes.ByteString

withOutput :: MonadUnliftIO m => FilePath -> (Consumer ByteString m () -> m a) -> m a
withOutput "-"  f = f stdout
withOutput path f = withBinaryFileDurable path WriteMode $ \h -> do
  f (toHandle h)
