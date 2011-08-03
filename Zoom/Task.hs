{-# LANGUAGE DeriveDataTypeable #-}
module Zoom.Task where
import Data.Typeable

data Args = Args String
            deriving (Show, Typeable)
                     
data ZoomTask a = Task {fromTask :: [Args] -> IO a}
                  deriving (Typeable)

