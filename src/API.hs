
{-# LANGUAGE OverloadedStrings #-}

module API 
  ( appendInBody
  , winInnerWidth
  , winInnerHeight
  , valToNumber
  ) where

import Control.Monad
import Control.Lens hiding ((#))
import Language.Javascript.JSaddle

appendInBody :: JSVal -> JSM ()
appendInBody v = void $ jsg "document" ^. js "body" ^. js1 "appendChild" v

winInnerWidth :: JSM Double
winInnerWidth = valToNumber =<< jsg "window"  ^. js "innerWidth"

winInnerHeight :: JSM Double
winInnerHeight = valToNumber =<< jsg "window"  ^. js "innerHeight"

