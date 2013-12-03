import PookieBathtime
import FRP.Sodium.GameEngine2D.GLUT
import FRP.Sodium.GameEngine2D.Platform (engine)


main = do
    game <- pookieBathtime
    engine (GLUTArgs "freecell" "resources") game
