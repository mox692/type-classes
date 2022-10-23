-- haskellの型クラスの再定義的な

--
-- Functor
--
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap' f [] = []
    fmap' f (h:t) = f h : fmap f t

--
-- Functor
--

--
-- Semigroup
--
class Semigroup' a where
    merge :: a -> a -> a

instance Semigroup' [a] where
    merge x y = x ++ y

instance Semigroup' a => Semigroup' (Maybe a) where
    merge Nothing x = x
    merge x Nothing = x
    merge (Just x) (Just y) = Just $ x `merge` y

--
-- Monoid
--
class Semigroup' a =>  Monoid' a where
    mempty' :: a
    mappend' :: a -> a -> a

instance  Monoid' [a] where
    mempty' = []
    mappend' = merge

--
-- Foldable
--
class Foldable' t where
    foldMap' :: Monoid' m => (a -> m) -> t a -> m

instance Foldable' [] where
    foldMap' f [] = mempty'
    foldMap' f (h:t) = f h `mappend'` foldMap' f t

--
-- Traversable
--
class (Functor t, Foldable t) => Traversable' t where
    traverse' :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable' [] where
    traverse' f [] = pure []
    traverse' f (h:t) = pure (:) <*> f h <*> traverse' f t
