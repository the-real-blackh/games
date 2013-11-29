module SimpleElement where


import FRP.Sodium.GameEngine2D.Platform

data Element = Platform PType | Spike | Coin
    deriving (Read, Show, Eq, Ord)

data PType = PLeft | PMiddle | PRight
    deriving (Read, Show, Eq, Ord)

instance Enum Element where
    toEnum 0 = Coin
    toEnum 1 = Platform PLeft
    toEnum 2 = Platform PMiddle
    toEnum 3 = Platform PRight
    toEnum 4 = Spike
    toEnum _ = error $ "Element.toEnum out of range"
    fromEnum Coin = 0
    fromEnum (Platform PLeft) = 1
    fromEnum (Platform PMiddle) = 2
    fromEnum (Platform PRight) = 3
    fromEnum Spike = 4

instance Bounded Element where
    minBound = Coin
    maxBound = Spike

loadElementResources :: Platform p =>
                        IO (Element -> Drawable p)
loadElementResources = do
    coin <- image "coin.png"
    platformLeft <- image "platform-left.png"
    platformMiddle <- image "platform-middle.png"
    platformRight <- image "platform-right.png"
    spike <- image "spike.png"
    return $ \e -> case e of
        Coin -> coin
        Platform PLeft -> platformLeft
        Platform PMiddle -> platformMiddle
        Platform PRight -> platformRight
        Spike -> spike

