data Mono a = Monoid [String] a

instance Semigroup (Mono a) where
  (<>) = undefined

instance Monoid a => Monoid (Mono a) where
  mempty = _

