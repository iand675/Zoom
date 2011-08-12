module Zoom.Task.Project where
import Zoom.Task
import Zoom.Demand
import System.Cmd
import System.Directory
import System.IO
import Prelude
import Control.Monad.Trans


mkdirAndInit = do
  makeDemand (String "dirname" "The directory/project name to initialize")
  retrieveKeyValuePairs
  dirname <- stringForKey "foo"
  liftIO $ do
    createDirectoryIfMissing True dirname
    setCurrentDirectory dirname
    system "git init"
    system "cabal init"
  return ()
init = simpleTask { desc = "initializes git and cabal project files"
                  , task = mkdirAndInit 
                  }
                    