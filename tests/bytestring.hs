import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  print (B.pack [99,97,110], B.pack [98..120])
  let by = B.pack [98,111,114,116]
  print (by, B.unpack by, B.length by, B.head by, B.last by)
  print (B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]])
  print (B.cons 85 (B.pack [80,81,82,84]), foldr B.cons B.empty [50..60])
  print (B.take 5 (B.cycle (B.pack [104,105])), LC.take 4 (LC.repeat 'z'), B.toChunks B.empty)
  print (C.words (C.pack "  hello  byte world "), C.lines (C.pack "x\ny\n"), C.unwords [C.pack "a", C.pack "b"])
  print (C.readInt (C.pack "-42 rest"), C.readInt (C.pack "+7"), C.readInt (C.pack "x1"), C.readInteger (C.pack "123456789012345678901"))
  print (C.split ',' (C.pack "a,b,,c"), S.split 44 S.empty, C.splitWith (== 'a') (C.pack "banana"))
  print (C.pack "h\233llo", C.unpack (C.pack "\200"), S.pack [255, 256, 300])
  print (S.foldl' (+) 0 (S.pack [1..10]), C.strip (C.pack "  x  "), C.reverse (C.pack "abc"))
  print (C.isPrefixOf (C.pack "ab") (C.pack "abc"), C.elemIndex 'c' (C.pack "abc"), C.count 'a' (C.pack "banana"))
  print (S.map (+1) (S.pack [65,66]), S.filter even (S.pack [1..9]), S.elem 66 (S.pack [65,66]))
  print (read "\"hi\"" :: C.ByteString, C.pack "a" < C.pack "b", mconcat [C.pack "a", C.pack "b"])
  print (C.spanEnd (== '!') (C.pack "hey!!"), C.breakEnd (== '/') (C.pack "a/b/c"), C.unsnoc (C.pack "ab"))
  print (sum (map fst (mapMaybe C.readInt (C.words (C.pack "1 2 3 x 4")))))
