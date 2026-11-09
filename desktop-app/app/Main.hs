{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import TextShow
import Monomer
import qualified Monomer.Lens as L

-- import Types
import UI
import Model
import Types
import Constants


main :: IO ()
main = do
  startApp (TodoModel initialTodos def TodoNone) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Todo list",
      appWindowIcon "./assets/images/icon.png",
      appTheme customDarkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appFontDef "Remix" "./assets/fonts/remixicon.ttf",
      appInitEvent TodoInit
      ]

