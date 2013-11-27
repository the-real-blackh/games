import Editor
import SimpleElement
import FRP.Sodium.GameEngine2D.GLUT
import FRP.Sodium.GameEngine2D.Platform (engine')
import System.Environment


main = do
    args <- getArgs
    case args of
        [fn] -> do
            res <- loadElementResources
            engine' (GLUTArgs "Level editor" "resources") $
                editor res fn
        _ -> fail $ "Usage: level-editor-glut <level>.hs"

