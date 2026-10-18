-- Control.Arrow, Data.Tree, Data.List.NonEmpty, Data.Text.Encoding,
-- Control.Monad.Random and Codec.Picture
import Control.Arrow
import Data.Tree
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Data.Word
import Control.Monad.Random
import Codec.Picture
import qualified Data.ByteString.Lazy as BL

t :: Tree Int
t = Node 1 [Node 2 [Node 4 []], Node 3 []]

main :: IO ()
main = do
  print (first (+1) (1, "a"), second length (1, "abc"), ((+1) *** (*2)) (3, 4), (show &&& (+1)) 5)
  print (map (fst &&& snd) [(1,2)], (length >>> (*2)) "abc", left (+1) (Left 3 :: Either Int Int))
  print t
  putStr (drawTree (fmap show t))
  print (flatten t, levels t, sum t, maximum t, foldTree (\x xs -> x + sum xs) t, fmap (*2) t == t)
  print (unfoldTree (\n -> (n, [m | m <- [2*n, 2*n+1], m < 7])) 1)
  let ne = 3 :| [1, 2]
  print (ne, NE.head ne, NE.toList (NE.sort ne), NE.length ne, sconcat (ne :| [4 :| []]))
  print (NE.group "aabccc", NE.nonEmpty ([] :: [Int]), fmap (+1) ne, sconcat ("ab" :| ["cd"]))
  print (sconcat (Max 3 :| [Max 7, Max 1]), stimes 3 "ab")
  let bs = TE.encodeUtf8 (T.pack "héllo")
  print (B.unpack bs, TE.decodeUtf8 bs, B.length bs)
  print (either (const Nothing) (Just . T.length) (TE.decodeUtf8' (B.pack [104, 195, 169])), either (const "invalid") T.unpack (TE.decodeUtf8' (B.pack [195])))
  -- the same seed gives the same numbers
  let rolls = evalRand (replicateM 10 (getRandomR (1, 6 :: Int))) (mkStdGen 7)
  print (rolls == evalRand (replicateM 10 (getRandomR (1, 6))) (mkStdGen 7), all (\r -> r >= 1 && r <= 6) rolls)
  xs <- evalRandIO (replicateM 100 (getRandomR (0, 1 :: Double)))
  print (all (\x -> x >= 0 && x <= 1) xs)
  c <- evalRandIO (fromList [('a', 1), ('b', 0)])
  print c
  -- a PNG: signature, chunks and size
  let png = BL.unpack (encodePng (generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 0) 4 3))
  print (take 8 png, take 4 (drop 12 png), pixelAt (generateImage (\x y -> fromIntegral (x * 10 + y) :: Pixel8) 5 5) 3 2)
