-- ref: https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Base.html

--
-- Functor
--
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
    fmap' f [] = []
    fmap' f (h:t) = f h : fmap f t

instance Functor' Maybe  where
    fmap' f Nothing = Nothing
    fmap' f (Just x) = Just $ f x


--
-- Applicative
--
class Applicative' f where
    pure' :: a -> f a
    apply' :: f (a -> b) -> f a -> f b -- In GHC.Base, it is called `<*>`.

instance Applicative' [] where
    pure' a = [a]
    apply' fs [] = []
    apply' [] xs = []
    apply' fs xs = [f x | f <- fs, x <- xs]

instance Applicative' Maybe where
    pure' a = Just a
    apply' f Nothing = Nothing
    apply' Nothing (Just x) = Nothing
    apply' (Just f) (Just x) = Just $ f x

--
-- Monad
--
class (Applicative m) => Monad' m where
    return' :: a -> m a
    bind' :: m a -> (a -> m b) -> m b

instance Monad' [] where
    return' = pure
    bind' a f = case a of
        [] -> []
        -- TODO: f h : bind' t f だとErrorだったけど何でだ？...
        h:t -> f h ++ bind' t f

instance Monad' Maybe where
    return' = pure
    bind' a f = case a of
        Nothing -> Nothing
        Just x -> f x

--
-- Semigroup
--
class Semigroup' a where
    merge' :: a -> a -> a -- In GHC.Base, it is called `<>`.

instance Semigroup' [a] where
    merge' x y = x ++ y

instance Semigroup' a => Semigroup' (Maybe a) where
    merge' Nothing x = x
    merge' x Nothing = x
    merge' (Just x) (Just y) = Just $ x `merge'` y

--
-- Monoid
--
class Semigroup' a =>  Monoid' a where
    mempty' :: a
    mappend' :: a -> a -> a

instance  Monoid' [a] where
    mempty' = []
    mappend' = merge'

instance Semigroup' a => Monoid' (Maybe a) where
    mempty' = Nothing
    mappend' = merge'

--
-- Foldable
-- 
class Foldable' t where
    -- その型の中身を取り出して、monoidに写すような関数が適用できる型であるか
    foldMap' :: Monoid' m => (a -> m) -> t a -> m

instance Foldable' [] where
    foldMap' f [] = mempty'
    foldMap' f (h:t) = f h `mappend'` foldMap' f t

instance Foldable' Maybe where
    foldMap' f Nothing = mempty'
    foldMap' f (Just x) = f x

--
-- Traversable
--

-- MEMO: モノイドの結合演算子 <> を、Applicativeな型 f で一般化した、という風に見ることができる
-- Ref: https://stackoverflow.com/questions/45798242/the-purpose-of-the-traversable-typeclass
class (Functor' t, Foldable' t) => Traversable' t where
    traverse' :: Applicative' f => (a -> f b) -> t a -> f (t b)
    sequenceA' :: Applicative' f => t (f a) -> f (t a)

instance Traversable' [] where
    traverse' f [] = pure' []
    traverse' f (h:t) = pure' (:) `apply'` f h `apply'` traverse' f t
    --ex1: traverse' (\s -> if s == 0 then Nothing else Just s) [1,2,3] ~> Just [1,2,3]
    --ex2: traverse' (\s -> if s == 0 then Nothing else Just s) [1,2,3,0] ~> Nothing
    --ex3: traverse' (\s -> [s]) $ Just 3 ~> [Just 3]  (あまり意味はなさそう)
    sequenceA' = traverse' id  -- id は GHC.Baseですでに定義されている
    --ex4: sequenceA' [Just 4, Just 5] ~> Just [4,5]


-- MEMO: 正直List以外のTraversable'以外の使い道がよくわからない
instance Traversable' Maybe where
    traverse' f Nothing = pure' Nothing
    traverse' f (Just x) = pure' Just `apply'` f x 

    sequenceA' = traverse' id
