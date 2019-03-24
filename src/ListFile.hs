{-# LANGUAGE NoImplicitPrelude #-}
module ListFile where

import Import

import Data.Char
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Read as LT

decodeListFile :: LText -> Either [String] [Int]
decodeListFile t = case lefts results of
  [] -> Right $ rights results
  es -> Left es
  where
    ls = map (LT.takeWhile isDigit)
       . LT.lines
       $ t
    results :: [Either String Int]
    results = do
      (lineNumber, str) <- zip [(1::Int)..] ls
      case LT.null str of
        True -> []
        False -> do
          case LT.decimal str of
            Left e       -> return $ Left $ "parse error at " <> show lineNumber <> ": " <> e
            Right (i, _) -> return $ Right i
