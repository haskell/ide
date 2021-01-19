--  see https://github.com/haskell/haskell-language-server/issues/663
module TLens where

import Optics

-- >>> (_1 %~ succ) (1, 2, 3)

-- >>> someFunc
someFunc :: (Int, Int, Int)
someFunc = (_1 %~ succ) (1, 2, 3)

data Point = Point {x, y :: Int} deriving (Show)

-- >>> lens y (\p i -> p {y = i}) %~ (+1) $ (Point 0 0)

-- >>> _Right %~ (-1+) $ Right 5 :: Either String Int
