-- Text.Printf: C-style formatted output.  printf is variadic thanks to the
-- PrintfType class, exactly as in GHC's implementation.
module Text.Printf (printf, hPrintf, PrintfType, PrintfArg(..), HPrintfType, UPrintf) where

import Husk.Base

data UPrintf = UInteger Integer | UChar Char | UString String | UDouble Double | UFloat Float

class PrintfArg a where
  toUPrintf :: a -> UPrintf

instance PrintfArg Int where
  toUPrintf n = UInteger (toInteger n)
instance PrintfArg Integer where
  toUPrintf = UInteger
instance PrintfArg Int8 where toUPrintf n = UInteger (toInteger n)
instance PrintfArg Int16 where toUPrintf n = UInteger (toInteger n)
instance PrintfArg Int32 where toUPrintf n = UInteger (toInteger n)
instance PrintfArg Word where toUPrintf n = UInteger (toInteger n)
instance PrintfArg Word8 where toUPrintf n = UInteger (toInteger n)
instance PrintfArg Word16 where toUPrintf n = UInteger (toInteger n)
instance PrintfArg Word32 where toUPrintf n = UInteger (toInteger n)
instance PrintfArg Word64 where toUPrintf n = UInteger (toInteger n)
instance PrintfArg Double where
  toUPrintf = UDouble
instance PrintfArg Float where
  toUPrintf = UFloat
instance PrintfArg Char where
  toUPrintf = UChar
instance PrintfArg [c] where
  toUPrintf s = UString (primUnsafeCoerce s)

class PrintfType r where
  spr :: String -> [UPrintf] -> r

instance PrintfType [c] where
  spr fmt args = primUnsafeCoerce (uprintf fmt (reverse args))

instance PrintfType (IO a) where
  spr fmt args = primUnsafeCoerce (putStr (uprintf fmt (reverse args)))

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
  spr fmt args = \a -> spr fmt (toUPrintf a : args)

printf :: PrintfType r => String -> r
printf fmt = spr fmt []

class HPrintfType r where
  hspr :: Handle -> String -> [UPrintf] -> r

instance HPrintfType (IO a) where
  hspr h fmt args = primUnsafeCoerce (hPutStr h (uprintf fmt (reverse args)))

instance (PrintfArg a, HPrintfType r) => HPrintfType (a -> r) where
  hspr h fmt args = \a -> hspr h fmt (toUPrintf a : args)

hPrintf :: HPrintfType r => Handle -> String -> r
hPrintf h fmt = hspr h fmt []

-- the formatter
uprintf :: String -> [UPrintf] -> String
uprintf "" [] = ""
uprintf "" _ = errorWithoutStackTrace "printf: formatting string ended prematurely"
uprintf ('%':'%':cs) us = '%' : uprintf cs us
uprintf ('%':cs) us =
  let (flags, cs1) = span (`elem` "-+ 0#") cs
      (width, cs2, us1) = getStar cs1 us
      (prec, cs3, us2) = case cs2 of
                           ('.':r) -> let (p, r', u') = getStar r us1 in (Just (maybe 0 id p), r', u')
                           _ -> (Nothing, cs2, us1)
      cs4 = dropWhile (`elem` "hlL") cs3
  in case cs4 of
       (c:rest) -> case us2 of
         (u:us3) -> fmtOne flags width prec c u ++ uprintf rest us3
         [] -> errorWithoutStackTrace "printf: argument list ended prematurely"
       [] -> errorWithoutStackTrace "printf: argument list ended prematurely"
uprintf (c:cs) us = c : uprintf cs us

getStar :: String -> [UPrintf] -> (Maybe Int, String, [UPrintf])
getStar ('*':cs) (UInteger n : us) = (Just (fromInteger n), cs, us)
getStar cs us = case span isDigit cs of
  ("", _) -> (Nothing, cs, us)
  (ds, rest) -> (Just (read ds), rest, us)

fmtOne :: String -> Maybe Int -> Maybe Int -> Char -> UPrintf -> String
fmtOne flags width prec c u =
  let leftAlign = '-' `elem` flags
      zeroPad = '0' `elem` flags && not leftAlign
      plus = '+' `elem` flags
      space = ' ' `elem` flags
      signed s = if plus && not (isNeg s) then '+' : s
                 else if space && not (isNeg s) then ' ' : s else s
      isNeg ('-':_) = True
      isNeg _ = False
      pad s = case width of
        Nothing -> s
        Just w | length s >= w -> s
               | leftAlign -> s ++ replicate (w - length s) ' '
               | zeroPad && isNumeric -> zeroFill s w
               | otherwise -> replicate (w - length s) ' ' ++ s
      zeroFill s w = case s of
        (sgn:r) | sgn `elem` "+- " -> sgn : replicate (w - length s) '0' ++ r
        _ -> replicate (w - length s) '0' ++ s
      isNumeric = c `elem` "dioxXbeEfFgGu"
      bad = errorWithoutStackTrace ("printf: bad formatting char " ++ show c)
  in pad $ case (c, u) of
       ('d', UInteger n) -> signed (show n)
       ('i', UInteger n) -> signed (show n)
       ('u', UInteger n) -> show n
       ('x', UInteger n) -> showHex n ""
       ('X', UInteger n) -> map toUpper (showHex n "")
       ('o', UInteger n) -> showOct n ""
       ('b', UInteger n) -> showBin n
       ('c', UChar ch) -> [ch]
       ('s', UString s) -> maybe s (\p -> take p s) prec
       ('s', UInteger n) -> show n
       ('s', UDouble d) -> show d
       ('s', UFloat d) -> show d
       ('s', UChar ch) -> [ch]
       ('v', UInteger n) -> show n
       ('v', UDouble d) -> showGFloat prec d ""
       ('v', UFloat d) -> showGFloat prec d ""
       ('v', UString s) -> s
       ('v', UChar ch) -> [ch]
       ('f', UDouble d) -> signed (showFFloat prec d "")
       ('F', UDouble d) -> signed (showFFloat prec d "")
       ('e', UDouble d) -> signed (showEFloat prec d "")
       ('g', UDouble d) -> signed (showGFloat prec d "")
       ('f', UFloat d) -> signed (showFFloat prec d "")
       ('F', UFloat d) -> signed (showFFloat prec d "")
       ('e', UFloat d) -> signed (showEFloat prec d "")
       ('g', UFloat d) -> signed (showGFloat prec d "")
       ('f', UInteger n) -> signed (showFFloat prec (fromInteger n :: Double) "")
       _ -> bad

showBin :: Integer -> String
showBin 0 = "0"
showBin n = reverse (go n)
  where go 0 = ""
        go k = (if odd k then '1' else '0') : go (k `div` 2)
