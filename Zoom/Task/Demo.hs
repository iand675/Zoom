module Zoom.Task.Demo where
import Zoom.Task

demo = simpleTask { desc = "Prints out the string, \"This is a demo!\"" 
                  , runTask = \ args -> do
                      putStrLn "This is a demo!" 
                  }

