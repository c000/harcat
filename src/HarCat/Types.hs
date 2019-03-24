{-# LANGUAGE TemplateHaskell #-}
module HarCat.Types where

import Data.Aeson.TH
import Data.Text
import Data.Vector

import Lens.Micro.Platform

import HarCat.Internal

data Content = Content
  { _encoding :: Maybe Text
  , _mimeType :: !Text
  , _size :: !Int
  , _text :: !Text
  } deriving Show

deriveJSON jsonDecodeOptions ''Content
makeLenses ''Content

data Request = Request
  { _url :: !Text
  } deriving Show

deriveJSON jsonDecodeOptions ''Request
makeLenses ''Request

data Response = Response
  { _content :: Content
  } deriving Show

deriveJSON jsonDecodeOptions ''Response
makeLenses ''Response

data Entry = Entry
  { _request :: !Request
  , _response :: !Response
  } deriving Show

deriveJSON jsonDecodeOptions ''Entry
makeLenses ''Entry

data Log a = Log
  { _entries :: Vector a
  } deriving Show

deriveJSON jsonDecodeOptions ''Log
makeLenses ''Log

data Har a = Har
  { _log :: Log a
  } deriving Show

deriveJSON jsonDecodeOptions ''Har
makeLenses ''Har
