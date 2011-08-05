module Zoom.Task.SayHello where
import Data.Pool
import Zoom.Task
import System.IO
import System.Directory
import Prelude
hi = putStrLn "hi"
  
hi2 = Task $ \args -> do
  cs <- getDirectoryContents "."
  putStrLn $ show cs



