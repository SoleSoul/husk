-- LYAH material found missing by an outside evaluation (husk 0.1.0)
import Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F

-- a Foldable instance that defines only foldMap
data Tree a = Empty | Node a (Tree a) (Tree a)

instance F.Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

testTree :: Tree Int
testTree = Node 5 (Node 3 (Node 1 Empty Empty) (Node 6 Empty Empty))
                  (Node 9 (Node 8 Empty Empty) (Node 10 Empty Empty))

-- newtype patterns don't evaluate their argument
newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

newtype Wrap = Wrap (Maybe Int)

unwrap :: Wrap -> Int
unwrap (Wrap (Just n)) = n
unwrap (Wrap Nothing) = 0

-- derived Read for records
data Car = Car { company :: String, model :: String, year :: Int } deriving (Show, Read, Eq)
data Shape = Circle { radius :: Double } | Rect Double Double deriving (Show, Read, Eq)
data Op = Op { (<+>) :: Int } deriving (Show, Read, Eq)

-- signatures of pattern bindings
a, b :: Int
(a, b) = (1, 2)
c :: Double
[c, _] = [1, 2]

main :: IO ()
main = do
  print (F.foldl (+) 0 testTree, F.foldr (:) [] testTree, product testTree)
  print (getAny (F.foldMap (\x -> Any (x == 3)) testTree), F.toList testTree, length testTree)
  print (maximum testTree, elem 8 testTree, sum testTree)
  putStrLn (helloMe undefined)
  print (case undefined of CoolBool _ -> 1, unwrap (Wrap (Just 4)), unwrap (Wrap Nothing))
  print (getZipList (fmap (+1) (ZipList [1,2,3])))
  print (getZipList ((+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]))
  print (getZipList (max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]))
  print (getZipList ((,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"))
  print (take 3 (getZipList (pure 7)), ZipList [True])
  let car = Car {company = "Ford", model = "Mustang", year = 1967}
  print (read (show car) == car)
  print (read "Car {company=\"Ford\", model = \"T\" , year= -3}" :: Car)
  print (read " ( Car {company = \"x\", model = \"y\", year = 1} ) " :: Car)
  print (map read ["Circle {radius = 2.5}", "Rect 1.0 2.0"] :: [Shape])
  print (read (show [Just car, Nothing]) :: [Maybe Car])
  print ((reads :: ReadS Car) "Car {company = \"x\"}")
  print (read (show (Op 3)) :: Op)
  print (a + b, c / 4)
