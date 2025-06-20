{-# LANGUAGE OverloadedStrings #-}

module API where

import Control.Monad
import Control.Lens ((^.))
import Language.Javascript.JSaddle as J

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

appendInBody :: JSVal -> JSM ()
appendInBody v = void $ jsg "document" ^. js "body" ^. js1 "appendChild" v

winInnerWidth :: JSM Double
winInnerWidth = valToNumber =<< jsg "window"  ^. js "innerWidth"

winInnerHeight :: JSM Double
winInnerHeight = valToNumber =<< jsg "window"  ^. js "innerHeight"

