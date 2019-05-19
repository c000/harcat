{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HarCat.Enumerator where

import Import

import Data.Aeson
import Data.Vector as V

import Text.Mustache

import Lens.Micro.Platform

import HarCat.Types

newtype EnumeratedEntries
  = EnumeratedEntries (Vector Entry)
  deriving Show

instance ToJSON EnumeratedEntries where
  toJSON (EnumeratedEntries vec)
    = toJSON $ V.imap enumeratedEntriesToJSON vec

enumeratedEntriesToJSON :: ToJSON e => Int -> e -> Value
enumeratedEntriesToJSON index entry = case toJSON entry of
  Object obj -> Object $ obj & at "index" .~ (Just $ toJSON index)
  _ -> error "unexpected type at (toJSON entry)"

formatEntries :: ToJSON e => Template -> Vector e -> [(LText, e)]
formatEntries template es = values
  where
    formatted = V.map (renderMustache template) . V.imap enumeratedEntriesToJSON $ es
    originals = es
    values = V.toList $ V.zip formatted originals
