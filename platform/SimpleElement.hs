module SimpleElement where


data Element = Platform PType | Spike | Coin
    deriving (Read, Show)

data PType = PLeft | PMiddle | PRight
    deriving (Read, Show)
