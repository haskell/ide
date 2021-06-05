data Expr = Apply Int Int

plus :: Expr -> Expr
plus (Apply n m) = Apply (n + m) 0
