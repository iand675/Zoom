module Main where
import Language.Haskell.Interpreter
import Control.Monad
import Control.Monad.Trans
  
main = do 
  result <- runInterpreter test
  putStrLn $ show result

isFunction x = case x of 
  Fun _ -> True
  _     -> False
  
msg x = liftIO . putStrLn . show $ x

files = ["/Users/ian/Code/zoom/Main.hs"]
test :: Interpreter ()
test = do
  loadModules files
  setTopLevelModules ["Main"]
  modules <- getLoadedModules
  exports <- mapM getModuleExports modules
  let fs = filter isFunction $ join exports
      fnames = map name fs
  msg fs
  mainType <- typeOf "main"
  msg mainType
