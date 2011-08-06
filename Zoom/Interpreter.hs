module Zoom.Interpreter where
import Language.Haskell.Interpreter
import qualified GHC
import PackageConfig
import UniqFM
import qualified HscTypes as GHC
import Packages
import Control.Monad
import Control.Monad.Trans
import System.Directory
import System.FilePath
import Data.Monoid
import Zoom.Task
import Data.Maybe
import qualified Data.List as L


ifM :: (Monad m, Monoid md) => m Bool -> md -> m md
ifM m x = do
  result <- m
  return $ if result
           then x
           else mempty

qualifyModule x  = L.stripPrefix "Zoom.Task." x
qualifyFunctions (m, fs) = map (qualifyFun qualifyAs) fs
  where qualifyAs = fromMaybe m (qualifyModule m)

defaultModules = [("Prelude", Nothing), ("Zoom.Task", Just "Zoom.Task")]

ghcGetAvailableModules :: GHC.GhcMonad m => m [GHC.ModuleName]
ghcGetAvailableModules = do
  dflags <- GHC.getSessionDynFlags
  let pkg_db = pkgIdMap (GHC.pkgState dflags)
  return $ concat (map exposedModules (filter exposed (eltsUFM pkg_db)))
    
getAvailableModules :: MonadInterpreter m => m [ModuleName]
getAvailableModules = liftM (map GHC.moduleNameString) $ runGhc ghcGetAvailableModules

-- | entry point for the standard zoom interpreter
interpreterMain :: [Args] -> Interpreter ()
interpreterMain args = do
  set [ languageExtensions := [TemplateHaskell, QuasiQuotes]
      , searchPath := ["./tasks"]]
  loadLocalTaskModules
  qualified <- importZoomTasks
  tasks <- availableTasks qualified
  dispatchArgs tasks args

qualifyFun q f = q ++ ('.':f)

filterTaskFuns :: [String] -> Interpreter [String]
filterTaskFuns fs = do
  tasks <- filterM (\f -> typeOf f >>= \t -> return $ L.isPrefixOf "Zoom.Task" t) fs
  return tasks

filterAppropriateTasks ts = do
  liftedTasks <- mapM (\x -> interpret ("Zoom.Task.discardType " ++ x) (as :: ZoomTask ())) ts
  appropriate <- liftIO $ filterM (enableIf . snd) (zip ts liftedTasks)
  return $ map fst appropriate
-- | loads up modules located in the task subdirectory of the current directory.
--   note that this currently needs to be run before loading global tasks.
loadLocalTaskModules :: Interpreter ()  
loadLocalTaskModules = do
  dirs           <- liftIO getTaskDirs
  allDirPaths    <- liftIO $ mapM getAndQualifyContents dirs
  allModulePaths <- liftIO $ filterM (fmap not . doesDirectoryExist) $ join allDirPaths
  loadModules allModulePaths
  
-- | imports both local and global Zoom.Task.* modules. 
--   returns the qualified module names of all Zoom.Task.* modules.
importZoomTasks :: Interpreter [ModuleName]
importZoomTasks = do
  localModules  <- getLoadedModules
  globalModules <- getAvailableModules
  let 
    zoomModules      = filter (L.isPrefixOf "Zoom.Task.") (localModules ++ globalModules)
    qualifiedModules = defaultModules ++ zip zoomModules (map qualifyModule zoomModules)
  setImportsQ qualifiedModules
  return zoomModules

getFunctionsFromImports :: [ModuleName] -> Interpreter [(ModuleName, [String])]
getFunctionsFromImports imps = do  
  exports <- mapM getModuleExports imps
  let pairs = zip imps $ map (map name . filter isFunction) exports
  return pairs
    
runZoomInterpreter :: [Args] -> IO (Either InterpreterError ())
runZoomInterpreter args = runInterpreter (interpreterMain args)


isFunction x = case x of 
  Fun _ -> True
  _     -> False
  
executeTask x = interpret ("\\args -> (Zoom.Task.runTask " ++ x ++ ") args >> return ()") (as :: [Args] -> IO ())

printTaskDescription taskName = do
  description <- interpret ("Zoom.Task.desc " ++ taskName) (as :: String)
  liftIO $ putStrLn description
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
  
availableTasks :: [String] -> Interpreter [String]
availableTasks qualified = do
  modsWithFuns <- getFunctionsFromImports qualified
  let qualifiedFuns = join $ map qualifyFunctions modsWithFuns
  allTasks <- filterTaskFuns qualifiedFuns
  filterAppropriateTasks allTasks

printAvailableTasks taskNames = do
  mapM_ (\t -> liftIO (putStr (t ++ ": ")) >> printTaskDescription t) $ taskNames

dispatchArgs availableTasks args = case args of
  [] -> do
    printAvailableTasks availableTasks
  (Args x):xs -> do 
    task <- executeTask x
    liftIO $ task xs