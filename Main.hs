module Main where
import Zoom.Interpreter
-- import System.Console.GetOpt         
import Zoom.Task
import System.Environment

main = do 
  args <- getArgs
  result <- runZoomInterpreter $ map Args args
  case result of
       Left err -> putStrLn $ show err
       Right _  -> return ()
