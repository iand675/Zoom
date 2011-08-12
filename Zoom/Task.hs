{-# LANGUAGE DeriveDataTypeable #-}
module Zoom.Task where
import Zoom.Demand
import Data.Typeable
import Control.Monad.State


type TaskTransformer a s = StateT s IO a
data Task s = Task { desc      :: String
                   , task      :: TaskTransformer () s
                   , enableIf  :: TaskTransformer Bool s
                   , dependsOn :: [Task s]
                   , alias     :: Maybe String}
            deriving (Typeable)
                     
runTask :: (Retrieve s) => s -> Task s -> IO ()
runTask s t = runStateT (runTask' t) s >> return ()
  
runTask' t = mapM_ runTask' (dependsOn t) >> task t

simpleTask :: Task a
simpleTask = Task undefined undefined (return True) [] Nothing
        
{-
Ideal task writing:
task = do
  makeDemand (String "foo" "bar")
  makeDemand (Bool "doThis" "should I do it?")
  retrieveKeyValuePairs
  foo <- stringForKey "foo"
  putStrLn foo
-}