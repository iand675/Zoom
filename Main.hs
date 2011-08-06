module Main where
import Language.Haskell.Interpreter
import Zoom.Interpreter
-- import System.Console.GetOpt         
import Zoom.Task
import System.Environment

main = do 
  args <- getArgs
  result <- runZoomInterpreter $ map Args args
  case result of
       Left err -> case err of
         UnknownError ue -> putStrLn ("Something went wrong, but I can't tell what happened. Here's all I know: " ++ ue)
         WontCompile errs -> do 
           putStrLn "Interpreting Error(s):"
           mapM_ (putStrLn . errMsg) errs                     
         NotAllowed str -> putStrLn $ "You can't do this: " ++ str
         GhcException e -> putStrLn $ "GHC Exception: " ++ e
       Right _  -> return ()
