class Monoid' a where
  mempty' :: a
  mappend' :: a -> a -> a

foldMap' :: (Foldable t, Monoid' m) => (a -> m) -> t a -> m
--foldMap' f = foldr (\a b -> f a `mappend'` b) mempty'
foldMap' f = foldr (mappend' . f) mempty'


newtype Endo' a = Endo' {appEndo' :: a -> a}

instance Monoid' (Endo' a) where
  mempty' = Endo' id
  (Endo' f) `mappend'` (Endo' g) = Endo' (f . g)

foldr' :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldr' f z as = appEndo' (foldMap' (Endo' . f) as) z


newtype Dual' a = Dual' {getDual' :: a}

instance (Monoid' a) => Monoid' (Dual' a) where
  mempty' = Dual' mempty'
  (Dual' f) `mappend'` (Dual' g) = Dual' (g `mappend'` f)

foldl' :: (Foldable t) => (b -> a -> b) -> b -> t a -> b
foldl' f z as = appEndo' (getDual' (foldMap' (Dual' . Endo' . flip f) as)) z

main :: IO ()
main = do
  print (foldr' (:) [] [1, 2, 3, 4])
  print (foldl  (flip (:)) [] [1, 2, 3])
  