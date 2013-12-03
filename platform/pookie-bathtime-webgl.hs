import PookieBathtime
import FRP.Sodium.GameEngine2D.WebGL
import FRP.Sodium.GameEngine2D.Platform (engine)


main = do
    game <- pookieBathtime
    engine (WebGLArgs "resources") game
