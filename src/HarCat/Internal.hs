module HarCat.Internal where

import Data.Aeson.TH

jsonDecodeOptions :: Options
jsonDecodeOptions = defaultOptions
  { fieldLabelModifier = tail
  }
