{-# LANGUAGE DeriveDataTypeable #-}
module Zoom.Task where
import Data.Typeable

data Args = Args String
            deriving (Show, Typeable)
                     
data ZoomTask a = Task { desc :: String
                       , runTask :: [Args] -> IO a
                       , enableIf :: IO Bool
                       , dependsOn :: [ZoomTask a]
                       , alias :: Maybe String }
                  deriving (Typeable)

simpleTask = Task undefined undefined (return True) [] Nothing

discardType t = t {runTask = discard, dependsOn = map discardType (dependsOn t)}
  where taskFun = runTask t
        discard args = taskFun args >> return ()