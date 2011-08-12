
module Main where
import Language.Haskell.Interpreter
import Zoom.Interpreter
-- import System.Console.GetOpt         
import Zoom.Task
import Zoom.Demand
import Zoom.Demand.CLI
import System.Environment
import Data.Typeable
main = do 
  args <- getArgs
  result <- runZoomInterpreter ([importFrom (undefined :: CLI)]) :: IO (Either InterpreterError [(String, Task CLI)])
  case result of
       Left err -> case err of
         UnknownError ue -> putStrLn ("Something went wrong, but I can't tell what happened. Here's all I know: " ++ ue)
         WontCompile errs -> do 
           putStrLn "Interpreting Error(s):"
           mapM_ (putStrLn . errMsg) errs                     
         NotAllowed str -> putStrLn $ "You can't do this: " ++ str
         GhcException e -> putStrLn $ "GHC Exception: " ++ e
       Right tasks  -> printTasks tasks

printTasks ts = mapM_ (putStrLn . fst) ts
-- filterAppropriateTasks :: (Typeable s, Retrieve s) => [String] -> [String]
-- filterAppropriateTasks ts = do
--  liftedTasks <- mapM (\x -> interpret ("Zoom.Task.discardType " ++ x) (as :: Task s)) ts
--  appropriate <- liftIO $ filterM (enableIf . snd) (zip ts liftedTasks)
--  return $ map fst appropriate