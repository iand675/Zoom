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

defaultModules = [("Prelude", Nothing), ("Zoom.Task", Just "Zoom.Task")]

interpreterMain = do
  set [ languageExtensions := [TemplateHaskell, QuasiQuotes]
      , searchPath := ["./tasks"]]
  msgs "Loading Modules..."
  loadTaskModules
  msgs "Modules Loaded."
  msgs "Running test..."
  test

isFunction x = case x of 
  Fun _ -> True
  _     -> False
  
msg x = liftIO . putStrLn . show $ x
msgs = liftIO . putStrLn

test :: Interpreter ()
test = do
  msgs "Scanning modules"
  modules <- getLoadedModules
  msgs "Getting exports"
  exports <- mapM getModuleExports modules
  msg exports
  let fs = filter isFunction $ join exports
      fnames = map name fs
  msg fs
  hi  <- interpret "SayHello.hi" (as :: IO ())
  liftIO hi
  hi2 <- interpretTask "SayHello.hi2"
  liftIO $ hi2 []
  
interpretTask x = interpret ("\\args -> (Zoom.Task.fromTask " ++ x ++ ") args >> return ()") (as :: [Args] -> IO ())

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
  msgs "Found task paths:"
  msg allModulePaths
  loadModules allModulePaths
--  setTopLevelModules defaultModules
  allModules     <- getLoadedModules
  msg allModules
  let 
    zoomModules      = filter (L.isPrefixOf "Zoom.Task.") allModules
    qualifyModule x  = L.stripPrefix "Zoom.Task." x
    qualifiedModules = defaultModules ++ zip zoomModules (map qualifyModule zoomModules)
  msg qualifiedModules
  setImportsQ qualifiedModules
