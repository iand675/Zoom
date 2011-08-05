module Main where
import Zoom.Interpreter
         
main = do 
  result <- runZoomInterpreter []
  case result of
       Left err -> putStrLn $ show err
       Right _  -> return ()
