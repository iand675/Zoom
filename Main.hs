module Main where
import Language.Haskell.Interpreter
import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.FilePath
import Data.Monoid
import Data.Typeable
import Zoom.Task
import qualified Data.List as L
ifM :: (Monad m, Monoid md) => m Bool -> md -> m md
ifM m x = do
  result <- m
  return $ if result
           then x
           else mempty
         
main = do 
  result <- runInterpreter interpreterMain
  putStrLn $ show result

interpreterMain = do
  set [installedModulesInScope := True]
  loadTaskModules
  test

isFunction x = case x of 
  Fun _ -> True
  _     -> False
  
msg x = liftIO . putStrLn . show $ x

files = ["/Users/ian/Code/zoom/Main.hs"]
test :: Interpreter ()
test = do
  modules <- getLoadedModules
  exports <- mapM getModuleExports modules
  let fs = filter isFunction $ join exports
      fnames = map name fs
  msg fs
  -- mainType <- typeOf "main"
  hi  <- interpret "SayHello.hi" (as :: IO ())
  hi2 <- interpretTask "SayHello.hi2"
  liftIO hi
  liftIO $ hi2 []
  
interpretTask x = interpret ("\\args -> (Task.fromTask " ++ x ++ ") args >> return ()") (as :: [Args] -> IO ())

-- get current working directory
-- TODO recurse all the way to home, getting tasks for each level.
-- TODO also, get them from some global location
getTaskDirs = do
  current <- getCurrentDirectory
  let taskDir = current </> "tasks"
  ifM (doesDirectoryExist taskDir) [taskDir]

getAndQualifyContents dir = do
  contents <- getDirectoryContents dir
  let realContents = filter (`notElem` [".", ".."]) contents
  return $ map (dir </>) realContents
  
loadTaskModules :: Interpreter ()
loadTaskModules = do
  dirs           <- liftIO getTaskDirs
  allDirPaths    <- liftIO $ mapM getAndQualifyContents dirs
  allModulePaths <- liftIO $ filterM (fmap not . doesDirectoryExist) $ join allDirPaths
  loadModules allModulePaths
  allModules     <- getLoadedModules
  let 
    zoomModules      = filter (L.isPrefixOf "Zoom.") allModules
    qualifyModule x  = L.stripPrefix "Zoom." x
    qualifiedModules = zip zoomModules (map qualifyModule zoomModules)
  setImportsQ qualifiedModules
--  setTopLevelModules zoomModules

-- type Task = ZoomTask IO 