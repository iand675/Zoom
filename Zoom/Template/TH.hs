{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Zoom.Template.TH (render, zoom, zoomFile) where
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.Romeo
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder

class ToZoomTemplate a where
    toZoom :: a -> Builder
instance ToZoomTemplate [Char] where toZoom = fromLazyText . TL.pack
instance ToZoomTemplate TS.Text where toZoom = fromText
instance ToZoomTemplate TL.Text where toZoom = fromLazyText

render x = x undefined

defaultZoomSettings = do
  wr   <- [|id|]
  unwr <- [|id|]
  toZoomExpr <- [| toZoom |]
  return $ defaultRomeoSettings {toBuilder = toZoomExpr, wrap = wr, unwrap = unwr}
  
zoom = QuasiQuoter {quoteExp = \s -> do
                       rs <- defaultZoomSettings
                       quoteExp (romeo rs) s
                   }
zoomFile fp = do  
  zs <- defaultZoomSettings
  romeoFile zs fp