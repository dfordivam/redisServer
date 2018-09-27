{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Reflex.Dom
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core (mainWidget, mainWidgetWithCss)
import Reflex.Dom hiding (mainWidget, run)
import TopWidget

main :: IO ()
main = run 3911 $ mainWidget $ topWidget

