module Zoom.Task.HelloWorld.Foo where
-- import Data.Pool
import Zoom.Task
import System.IO
import System.Directory
import Prelude
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Monad.Trans
hi = putStrLn "hi"

build = simpleTask { desc = "builds something"
                   , enableIf = liftIO $ doesFileExist "CMakeLists.txt" 
                   , task = liftIO $ putStrLn "I could do interesting things, but I don't!"
                   }

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/12),cos(2*pi*k/12),0.0)) [1..12]

runGL = simpleTask { desc = "runs a GL window" 
                   , task = liftIO $ do
                     (progname, _) <- getArgsAndInitialize
                     createWindow "foo"
                     displayCallback $= display
                     mainLoop
                   }
  
display = do 
  clear [ColorBuffer]
  renderPrimitive Points $ mapM_ (\(x, y, z)->vertex$Vertex3 x y z) myPoints
  flush
