module Zoom.Task.Demo where
import Zoom.Task
import Zoom.Demand
import Control.Monad.Trans
import Control.Monad.State

demo :: Retrieve s => Task s
demo = simpleTask { desc = "Prints out the string, \"This is a demo!\"" 
                  , task = flexDemo
                  }
       
yourName = String "name" "Your name"

flexDemo :: Retrieve s => TaskTransformer () s
flexDemo = do
  source <- get
  name <- liftIO $ retrieve source yourName
  liftIO $ putStrLn $ "Hi, " ++ (value name) ++ "!"