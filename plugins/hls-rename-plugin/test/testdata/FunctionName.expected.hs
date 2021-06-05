main = do
    x <- return $ baz 42
    return (baz x)
baz :: Int -> Int
baz x = x + 1
bar = (+ 1) . baz
