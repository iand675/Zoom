module Zoom.Task.Demo where
import Zoom.Task

demo = Task "Prints out the string, \"This is a demo!\"" $ \ args -> do
  putStrLn "This is a demo!"

