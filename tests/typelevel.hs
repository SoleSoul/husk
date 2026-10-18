-- monad transformers, polymorphic recursion, higher-kinded type parameters
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \s -> do
    (a, s') <- g s
    return (f a, s')

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  StateT mf <*> StateT ma = StateT $ \s -> do
    (f, s1) <- mf s
    (a, s2) <- ma s1
    return (f a, s2)

instance Monad m => Monad (StateT s m) where
  StateT m >>= k = StateT $ \s -> do
    (a, s') <- m s
    runStateT (k a) s'

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> m >>= \a -> return (a, s)

getS :: Monad m => StateT s m s
getS = StateT $ \s -> return (s, s)

putS :: Monad m => s -> StateT s m ()
putS s = StateT $ \_ -> return ((), s)

counterDemo :: StateT Int IO ()
counterDemo = do
  n <- getS
  lift (putStrLn ("count = " ++ show n))
  putS (n + 1)
  m <- getS
  lift (print m)

-- polymorphic recursion needs the signature
data Nested a = Flat a | Nest (Nested [a])

depth :: Nested a -> Int
depth (Flat _) = 0
depth (Nest n) = 1 + depth n

-- higher-kinded parameters
newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap h (Compose x) = Compose (fmap (fmap h) x)

newtype Fix f = Fix (f (Fix f))

data ListF a r = NilF | ConsF a r

instance Functor (ListF a) where
  fmap _ NilF = NilF
  fmap f (ConsF a r) = ConsF a (f r)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (Fix x) = alg (fmap (cata alg) x)

fromList :: [a] -> Fix (ListF a)
fromList = foldr (\a r -> Fix (ConsF a r)) (Fix NilF)

sumAlg :: ListF Int Int -> Int
sumAlg NilF = 0
sumAlg (ConsF a r) = a + r

-- class with a polymorphic method and a superclass default
class Functor f => Box f where
  unbox :: f a -> a
  rebox :: a -> f a
  mapBox :: (a -> b) -> f a -> f b
  mapBox g = rebox . g . unbox

newtype Id a = Id a deriving Show
instance Functor Id where fmap g (Id a) = Id (g a)
instance Box Id where
  unbox (Id a) = a
  rebox = Id

main :: IO ()
main = do
  ((), final) <- runStateT counterDemo 41
  print final
  print (depth (Nest (Nest (Flat [[1 :: Int]]))))
  print (getCompose (fmap (+ 1) (Compose [Just 1, Nothing, Just 3])))
  print (cata sumAlg (fromList [1 .. 10]))
  print (mapBox show (Id 5), unbox (rebox 'x' :: Id Char))
