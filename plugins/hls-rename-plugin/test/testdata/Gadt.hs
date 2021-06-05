{-# LANGUAGE GADTs #-}

data Expression a where
    Number  :: Int -> Expression Int
    Boolean :: Bool -> Expression Bool
    Not     :: Expression Bool -> Expression Bool
    Even    :: Expression Int -> Expression Bool
    Add     :: Enum a => Expression a -> Expression a -> Expression Int
    Max     :: Ord a => Expression a -> Expression a -> Expression a

evaluate :: Expression a -> a
evaluate (Number n)  = n
evaluate (Boolean p) = p
evaluate (Add n m)   = fromEnum (evaluate n) + fromEnum (evaluate m)
evaluate (Even n)    = even $ evaluate n
evaluate (Not p)     = not $ evaluate p
evaluate (Max x y)   = max (evaluate x) (evaluate y)
