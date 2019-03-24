{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_harcat

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_harcat.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> strOption ( long "input"
                    <> short 'i'
                    <> metavar "FILE"
                    <> help "Input har filepath"
                     )
       <*> strOption ( long "output"
                    <> short 'o'
                    <> metavar "FILE"
                    <> value "-"
                    <> showDefault
                    <> help "Output filepath (\"-\" as stdout)"
                     )
       <*> option (Just <$> maybeReader Just) ( long "use-list"
                                             <> short 'L'
                                             <> value Nothing
                                             <> help "List file of response indexes (\"-\" as stdin)"
                                              )
       <*> strOption ( long "template"
                    <> metavar "TEMPLATE"
                    <> value "{{# response.content.encoding }}{{ index }};\t{{{ request.url }}}{{/ response.content.encoding }}"
                    <> showDefault
                    <> help "Mustache template for listing"
                    <> hidden
                     )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
