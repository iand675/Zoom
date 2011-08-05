module Zoom.Task.Demo where
import Zoom.Task

demo = Task $ \ args -> do
  putStrLn "This is a demo!"

