{-# LANGUAGE GADTs #-}

data Expr a where
    Number  :: Int -> Expr Int
    Boolean :: Bool -> Expr Bool
    Not     :: Expr Bool -> Expr Bool
    Even    :: Expr Int -> Expr Bool
    Add     :: Enum a => Expr a -> Expr a -> Expr Int
    Max     :: Ord a => Expr a -> Expr a -> Expr a

evaluate :: Expr a -> a
evaluate (Number n)  = n
evaluate (Boolean p) = p
evaluate (Add n m)   = fromEnum (evaluate n) + fromEnum (evaluate m)
evaluate (Even n)    = even $ evaluate n
evaluate (Not p)     = not $ evaluate p
evaluate (Max x y)   = max (evaluate x) (evaluate y)
