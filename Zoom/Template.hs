{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Zoom.Template (render, zoom, zoomFile) where
import Zoom.Template.TH

exampleHelper :: Int
exampleHelper = 21

example foo = render [zoom|Hi, my name is #{"Ian"}.
I am #{show exampleHelper} years old.
I also would like to say, "#{foo}".|]
