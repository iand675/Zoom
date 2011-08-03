module Zoom.SayHello where
import Zoom.Task

hi = putStrLn "hi"
  
hi2 = Task $ \args -> do
  putStrLn "Hi"



