module B (a, b, F(g)) where

class F a where
    g :: a

instance F Integer where
    g = 3

instance F Bool where
    g = True

a = 3
b = 4
c = 5